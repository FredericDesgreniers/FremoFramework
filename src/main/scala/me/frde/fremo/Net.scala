package me.frde.fremo

import com.fasterxml.jackson.core.JsonParser

import java.io.{BufferedReader, DataInputStream, DataOutputStream, File, InputStream, InputStreamReader, OutputStream}
import java.net.{ServerSocket, Socket, URI, URL, URLEncoder}
import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.nio.file.{FileSystems, Path, Paths}
import java.util
import java.util.{Collections, UUID}
import java.util.logging.{ConsoleHandler, Level, LogManager}
import javax.net.ssl.HttpsURLConnection
import scala.collection.mutable
import scala.util.matching.Regex

object Net

case class HttpRequest(
                        socket: Socket,
                        requestType: String,
                        route: String,
                        httpVersion: String,
                        headers: scala.collection.Map[String, String],
                        queryParams: scala.collection.Map[String, String] = Map(),
                        cookies: scala.collection.Map[String, String] = Map(),
                        body: String = "",
                        matchContext: Option[RouteMatchContext] = None):
  def outputStream = socket.getOutputStream
  def writeResponse(content: String, returnCode: String = "200 OK", headers: Map[String, String] = Map()): Unit =
    writeResponse(returnCode, Map("Content-Type" -> "text/html") ++ headers, content.getBytes)

  def writeResponse(returnCode: String, headers: Map[String, String], content: Array[Byte]): Unit =
    val headerbytes = (headers + (("Content-Length", content.length.toString))).map((tuple) => tuple._1 + ": " + tuple._2).mkString("\r\n").getBytes
    val returnHead = s"HTTP/1.1 ${returnCode}\r\n"
    println(s"Returning: ${returnHead}")
    outputStream.write(returnHead.getBytes)
    outputStream.write(headerbytes)
    outputStream.write('\r')
    outputStream.write('\n')
    outputStream.write('\r')
    outputStream.write('\n')
    outputStream.write(content)
    outputStream.flush()

object HttpRequest:
  def from(socket: Socket) =
    val reader = new BufferedReader(new InputStreamReader(socket.getInputStream))

    val (requestType, routeWithParams, httpType) = {
      val requestTypeLine = reader.readLine()
      val requestTypeParts = requestTypeLine.split(' ')
      println(requestTypeParts.mkString("Array(", ", ", ")"))
      (requestTypeParts(0), requestTypeParts(1), requestTypeParts(2))
    }

    val (route, params) = {
      val querySeperatorIndex = routeWithParams.indexOf("?")
      if(querySeperatorIndex == -1) then
        (routeWithParams, Map())
      else
        val (r, p) = routeWithParams.splitAt(querySeperatorIndex)
        (r, p.substring(1).split("&").map(param => {
            val paramParts = param.split("=")
            (paramParts(0), paramParts(1))
        }).toMap)
    }

    val headers = mutable.Map[String, String]()
    var str = reader.readLine()
    while(str != null && str != "") do
      val (name, value) = str.splitAt(str.indexOf(':'))
      headers.put(name.toLowerCase, value.substring(1).trim)
      str = reader.readLine()


    val cookies = headers.get("cookie").map(cookiesStr => {
      val cookies = cookiesStr.split(";")
      cookies.map(cookieStr => {
        val cookieParts = cookieStr.split("=")
        (cookieParts(0), cookieParts(1))
      }).toMap
    }).getOrElse(Map[String, String]())

    val request = HttpRequest(socket, requestType, route, httpType, headers, params, cookies)
    println(request)
    request

case class HttpRequestSource(port: Int = 80) extends Source[HttpRequest]:
  override def startLoop(): Unit =
    val serverSocket = new ServerSocket(port)
    while true do
      val socket = serverSocket.accept()
      Fremo.spawn {
        try
          publish(HttpRequest.from(socket))

          if !socket.isClosed then
            socket.close()
        catch
          case e: Exception => e.printStackTrace()
      }

case class Router(mapping: (Route | RoutePart, Target[RouterOutput])*) extends Source[HttpRequest] with Target[HttpRequest]:
  val routes = mapping.map((rt, target) =>
    rt match
      case part: RoutePart => (Route(Seq(part), "GET"), target)
      case route: Route => (route, target)
  )
  override def start(): Unit = ()
  override def react(request: HttpRequest): Unit =
    val inputRoute = if request.route.startsWith("/") then
      request.route.substring(1).trim
    else
      request.route.trim
    val result = routes.map {
      (route, target) =>
        val result = route.getMatch(inputRoute, request.requestType)
        (result, target)
    }.find((context, target) => context.valid == true)

    result match
      case Some((context, target)) =>
        try
          target.react(RouterOutput(request, context))
        catch
          case e: Throwable => e.printStackTrace()
      case None =>
        NotFoundHttpProcessor.react(RouterOutput(request, RouteMatchContext(false, Map())))


case class RouteMatchContext(valid: Boolean, variableBinding: Map[String, String])

object Route:
  def apply(requestType: String="GET"): Route = Route(Seq(), requestType)

  def parse(str: String, requestType: String = "GET") = {
    val parts = str.split("/").map(s =>
      if s.startsWith("$") then
        Variable(s.substring(1))
      else
        Literal(s)
    ).toSeq

    Route(parts, requestType)
  }

case class RouterOutput(request: HttpRequest, context: RouteMatchContext)

case class Route(parts: Seq[RoutePart], requestType: String):
  def getMatch(path: String, requestTypeToMatch: String): RouteMatchContext =
    if !requestTypeToMatch.equalsIgnoreCase(requestType) then return RouteMatchContext(false, Map())

    val pathParts = if path.isEmpty then Array[String]() else path.split("/")

    if pathParts.size != parts.size then
      return RouteMatchContext(false, Map())

    if pathParts.size == 0 && parts.size == 0 then
      return RouteMatchContext(true, Map())

    parts.zip(pathParts).map(
      (part, input) =>
        part match
          case Literal(lit) => RouteMatchContext(lit.equals(input), Map())
          case Variable(name) => RouteMatchContext(true, Map(name -> input))
    ).fold(RouteMatchContext(true, Map()))((a: RouteMatchContext, b: RouteMatchContext) =>
      a.copy(
        valid = a.valid && b.valid,
        variableBinding = a.variableBinding ++ b.variableBinding
      )
    )

  def /(rhs: RoutePart) = this.copy(
    parts = parts.appended(rhs)
  )
  def /(rhs: String) = this.copy(
    parts = parts.appended(Literal(rhs))
  )

  override def toString: String = parts.mkString("/")

sealed trait RoutePart:
  val name: String
  def /(rhs: RoutePart) = Route(Seq(this, rhs), "GET")
  def /(rhs: String) = Route(Seq(this, Literal(rhs)), "GET")


given Conversion[RoutePart, Route] with
  override def apply(x: RoutePart): Route = Route(Seq(x), "GET")

sealed case class Literal(val name: String) extends RoutePart:
  override def toString: String = name

sealed case class Variable[T](val name: String) extends RoutePart:
  override def toString: String = name

case class HttpOptions(val alwaysReadFiles: Boolean = true)

case class RootHttpProcessor() extends Target[RouterOutput]:
  override def react(input: RouterOutput): Unit =
    println(input)
    input.request.writeResponse("Hello World")

object NotFoundHttpProcessor extends Target[RouterOutput]:
  override def react(input: RouterOutput): Unit =
    input.request.writeResponse("Page not found...", returnCode = "404 NOT FOUND")

object Files {
  def readStringFromFile(path: String, file: java.io.File): String = {
    if(file.exists()) then
      java.nio.file.Files.readString(file.toPath)
    else {
      val uri = this.getClass.getResource("/"+path).toURI
      if(uri.toString.contains("!")) {
        val env: util.Map[String, String] = Collections.emptyMap()
        val array = uri.toString.split("!")
        val fs = FileSystems.newFileSystem(URI.create(array(0)), env)
        val path = fs.getPath(array(1))
        val content = java.nio.file.Files.readString(path)
        fs.close()
        content
      } else {
        java.nio.file.Files.readString(Path.of(uri))
      }
    }
  }
}

case class FilesHttpProcessor(directoryPath: String) extends Target[RouterOutput]:
  override def react(input: RouterOutput): Unit =
    val fileName = input.context.variableBinding("fileName")
    val filePath = directoryPath + "/" + fileName
    val file = new java.io.File(filePath)
    val content: String = Files.readStringFromFile(filePath, file)
    println(content)
    input.request.writeResponse(content)

case class FileHttpProcessor(filePath: String)(implicit options: HttpOptions) extends Target[RouterOutput]:
  val file = new java.io.File(filePath)

  private val content: String = Files.readStringFromFile(filePath, file)

  override def react(input: RouterOutput): Unit = input.request.writeResponse(content)


case class WritePostProcessor() extends Target[RouterOutput]:
  override def react(input: RouterOutput) =
    val user = Auth.getAuthUser(input.request)
    input.request.writeResponse(s"${user.session_token} is session id of ${user.username} with user id ${user.user_id}")

case class ReturnCurrentUserPreprocessor() extends Target[RouterOutput]:
  override def react(input: RouterOutput) =
    val user = Auth.getAuthUser(input.request)
    input.request.writeResponse(s"${user.username}")


implicit val options: HttpOptions = HttpOptions()

object NetTest:
  def main(args: Array[String]): Unit =
    val httpRequestSource = HttpRequestSource(port = 80)
    val rootProcessor = RootHttpProcessor()

    Reactive.react(
      httpRequestSource --> Router(
        Route() -> rootProcessor,
        Route.parse("blog") -> FileHttpProcessor("webFiles/blog.html"),
        Route.parse("login") -> LoginHttpProcessor(),
        Route.parse("blog/$fileName") -> FilesHttpProcessor("webFiles/entries"),
        Route.parse("js/$fileName") -> FilesHttpProcessor("webFiles/js"),
        Route.parse("write") -> WritePostProcessor(),
        Route.parse("writePost", "POST") -> WritePostProcessor(),
        Route.parse("user") -> ReturnCurrentUserPreprocessor())
    ).start()
    println("Net loop started")

