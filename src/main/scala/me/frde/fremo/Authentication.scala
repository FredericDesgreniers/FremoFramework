package me.frde.fremo

import org.json4s.*
import org.json4s.native.JsonMethods.*

import java.io.{BufferedReader, DataInputStream, DataOutputStream, InputStreamReader}
import java.net.{URL, URLEncoder}
import java.util.UUID
import javax.net.ssl.HttpsURLConnection

case class AuthUser(user_id: String, username: String, session_token: String, current_google_token: String) extends Entity[String]:
  override def key: String = user_id

object Auth:
  def getAuthUser(request: HttpRequest): AuthUser =
    val user = DB.getByProperty[AuthUser]("session_token", request.cookies("fremo-session"))
    user.get

case class LoginHttpProcessor() extends Target[RouterOutput]:
  override def react(item: RouterOutput): Unit =
    val maybeError = item.request.queryParams.get("error")
    if maybeError.isDefined then
      println(s"Got error from google... ${maybeError.get}")
      item.request.writeResponse("Google gave back an getAuthUser error...")

    val googleCode = item.request.queryParams("code")
    println(s"Got google code ${googleCode}, will try to ask google for token")
    val user = getAuthUser(googleCode)

    item.request.writeResponse("Going back to blog", "303", Map("Location" -> "/blog", "Set-Cookie" -> s"fremo-session = ${user.session_token}"))


  def encode(str: String) = URLEncoder.encode(str, "UTF-8")

  def sendGoogleApiRequest(query: String, body: String, httpType: String = "GET") =
    val authUrl = new URL(query + (if httpType == "GET" then s"?${body}" else ""))
    val connection = authUrl.openConnection().asInstanceOf[HttpsURLConnection]
    connection.setRequestMethod(httpType)
    connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")
    connection.setRequestProperty("Content-Length", body.length.toString)
    connection.setDoOutput(true);
    if(httpType != "GET") then
      connection.setDoInput(true);

      val outputStream = new DataOutputStream(connection.getOutputStream)
      println(s"Sending body ${body}")
      outputStream.writeBytes(body)
      outputStream.close()

    val stream = if connection.getResponseCode >= 400 then
      connection.getErrorStream
    else connection.getInputStream
    println(stream)
    val inputStream = new DataInputStream(stream)
    val reader = new BufferedReader(new InputStreamReader(inputStream))
    var line = reader.readLine()
    var response = ""
    while line != null do
      response += line
      line = reader.readLine()
    println(response)
    val jsonResponse: JValue = parse(response)

    jsonResponse

  def getStr(value: JValue) = value match
    case org.json4s.JString(str) => str
    case _ => ""

  def getAuthUser(code: String): AuthUser =
    val clientId = DB.get[Setting]("google.clientID").get.settingValue
    val clientSecret = DB.get[Setting]("google.clientSecret").get.settingValue
    val googleApiBase = "https://oauth2.googleapis.com"
    val tokenRequestQuery = s"${googleApiBase}/token"
    val tokenRequestBody = s"client_id=${encode(clientId)}&client_secret=${encode(clientSecret)}&code=${code}&grant_type=authorization_code&redirect_uri=${encode("http://local.frde.me/login")}"

    val tokenResponse = sendGoogleApiRequest(tokenRequestQuery, tokenRequestBody, httpType = "POST")

    val access_token = getStr(tokenResponse \\ "access_token")
    println(s"Got acccess token ${access_token}")
    //googleapis.com/oauth2/v1/userinfo?alt=json&access_token=youraccess_token
    val userInfoResponse = sendGoogleApiRequest(s"https://www.googleapis.com/oauth2/v1/userinfo", s"alt=json&access_token=${access_token}")
    println(userInfoResponse)

    val id = getStr(userInfoResponse \\ "id")
    val name = getStr(userInfoResponse \\ "name")

    val user = DB.get[AuthUser](id) match
      case Some(user) => user.copy(
        current_google_token = access_token,
        session_token = UUID.randomUUID().toString
      )
      case None => {
        val newUser = AuthUser(id, name, UUID.randomUUID().toString, access_token)
        newUser
      }
    DB.put(user)
    user

