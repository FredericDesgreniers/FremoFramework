package me.frde.fremo

import java.io.*
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode
import java.nio.file.{OpenOption, Paths, StandardOpenOption}
import java.time.{LocalDateTime, ZoneId, ZoneOffset}
import java.util.concurrent.{CompletableFuture, CompletionStage}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.quoted.{Expr, Quotes, Type}
import scala.reflect.{ClassTag, classTag}
import Conversion.conversion

import java.lang.reflect.{Field, InvocationHandler, Method}
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoField, TemporalField}
import java.util
import java.util.UUID





trait Ref[E <: Entity[_]]:
  def load(): E

object Conversion {
  given conversion[E <: Entity[_]]: Conversion[Ref[E], E] with
    override def apply(x: Ref[E]): E = x.load()
}

case class LocalRef[E <: Entity[_]](entity: E) extends Ref[E] :
  override def load(): E = entity


case class User(name: String) extends Entity[String] :
  override def key: String = name





object BitemporalSurface:
  def now() =
    val now = LocalDateTime.now()
    BitemporalSurface(now, now)


case class BitemporalSurface(vt: LocalDateTime, tt: LocalDateTime):
  def vtEpoch = vt.atZone(ZoneId.systemDefault()).toEpochSecond
  def ttEpoch = vt.atZone(ZoneId.systemDefault()).toEpochSecond

object DB:
  def put[E <: Entity[K], K](entity: E)(implicit evidence: ClassTag[E]): Unit =
    val serializedEntity = serialize(entity)
    CosmoDB.put(serializedEntity)

  def getByProperty[E](name: String, value: String)(implicit evidence: ClassTag[E]): Option[E] =
    val cosmoResults = CosmoDB.getByProperty(evidence.runtimeClass.getName, name, value)
    println(s"Got ${cosmoResults.size()} results")
    println(cosmoResults)

    if(cosmoResults.size() == 0)
      return None

    val jsonEntity = cosmoResults.get(cosmoResults.size()-1)
    deserializeEntity(jsonEntity)

  def get[E <: Entity[_]](key: Entity.Key[E])(implicit evidence: ClassTag[E]): Option[E] =
    val cosmoResults = CosmoDB.getByKey(key.toString, evidence.runtimeClass.getName)
    println(s"Got ${cosmoResults.size()} results")
    println(cosmoResults)

    if(cosmoResults.size() == 0)
      return None

    val jsonEntity = cosmoResults.get(cosmoResults.size()-1)
    deserializeEntity(jsonEntity)

  private def deserializeEntity[E](json: java.util.Map[String, Object])(implicit evidence: ClassTag[E]) =
    val entityClass: Class[E] = evidence.runtimeClass.asInstanceOf[Class[E]]
    val constructor = entityClass.getDeclaredConstructors.head

    val parameters = constructor.getParameters.flatMap(param => {
      val fieldValue = json.get(param.getName)
      println(s"${param.getName} - ${fieldValue}")
      deserialize(json.get(param.getName))
    })

    val entity = constructor.newInstance(parameters: _*)

    entityClass.getDeclaredFields.foreach((field: Field) => {
      val fieldValue = json.get(field.getName)
      println(s"${field.getName} - ${fieldValue}")
      val value = deserialize(fieldValue)
      value match
        case Some(v) =>
          field.setAccessible(true)
          field.set(entity, v)
        case None => ()
    })
    Some(entity.asInstanceOf[E])


  private def deserialize(value: Object) =
    value match
      case str: String => Some(str)
      case _ =>
        val map = value.asInstanceOf[util.Map[String, String]]
        val v = map.get("value")
        map.get("type").toLowerCase match
          case "int" => Some(v.toInt)
          case "java.lang.string" => Some(v)
          case "boolean" => Some(v.toBoolean)
          case _ => None

  private def serialize[E](entity: E)(implicit evidence: ClassTag[E], entityEvidence: ClassTag[Entity[_]]): java.util.Map[String, Object] = {
    val runtimeClass = evidence.runtimeClass
    val idField = runtimeClass.getDeclaredField("id")
    idField.setAccessible(true)
    import scala.jdk.CollectionConverters._
    val keymethod = runtimeClass.getDeclaredMethod("key")

    (runtimeClass.getDeclaredFields.map(field => {
      field.setAccessible(true)
      if field.get(entity).getClass == entityEvidence.runtimeClass then
        (field.getName, Map(
          "id" -> field.get(entity).asInstanceOf[Entity[_]].id,
          "type" -> "Entity"
        ).asJava)
      else
        (field.getName, Map(
          "value" -> serializeField(field.get(entity)),
          "type" -> field.getType.getName
        ).asJava)
    }).toMap ++ Map[String, Object](
      "id" -> idField.get(entity),
      "type" -> runtimeClass.getName,
      "key" -> keymethod.invoke(entity).toString,
      "vt" -> Fremo.currentScenario.get().surface.vtEpoch,
      "tt" -> Fremo.currentScenario.get().surface.ttEpoch
    )).asJava
  }

  private def serializeField[T](value: T)(implicit evidence: ClassTag[Ref[_]]): Object =
    value.toString

trait EntityValueHandler[T]:
  def serialize(value: T): String
  def deserialize(input: String): T

object StringValueHandler extends EntityValueHandler[String]:
  override def serialize(value: String): String = ???

  override def deserialize(input: String): String = ???

class EntityInvocationHandler(map: java.util.Map[String, Object]) extends InvocationHandler:
  override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
    method



object LocalDB:
  def loadProperty(name: String) =
    ???
    
  def getByRef(`type`: String, name: String, )