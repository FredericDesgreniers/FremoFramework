package me.frde.fremo

import java.io.Serializable
import java.util.UUID
import scala.reflect.ClassTag

sealed trait TransformerOperation

final case class AddField[T](name: String, data: T)(implicit evidence: ClassTag[T]) extends TransformerOperation:
  val typeName = evidence.runtimeClass.getName

final case class RemoveField(name: String) extends TransformerOperation

type SerializedEntity = Map[String, Object]
type SerializedEntityMut = scala.collection.mutable.Map[String, Object]

case class EntityTransformer[I, O](operations: TransformerOperation*)(implicit oCls: ClassTag[O], iCls: ClassTag[I]):
  val IShape = Shape.makeShape[I]
  val OShape = Shape.makeShape[O]

  def transform(input: SerializedEntity): SerializedEntity =
    val transformingInput: SerializedEntityMut = scala.collection.mutable.Map.from(input)

    operations.foreach(_ match
      case addField: AddField[_] => transformingInput += (addField.name -> Map(
        "value" -> addField.data,
        "type" -> addField.typeName
      ))
      case removeField: RemoveField => transformingInput.remove(removeField.name)
    )

    Map.from(transformingInput)

object Shape:
  def makeShape[T](implicit evidence: ClassTag[T]): Shape =
    Shape(evidence.runtimeClass.getDeclaredFields.map(
      field =>
        ShapeSegment(field.getName, field.getType.getName)
    ).toSeq)

case class Shape(segments: Seq[ShapeSegment])

case class ShapeSegment(name: String, kind: String)

trait Entity[Key] extends Serializable :
  def key: Key

  val id: String = UUID.randomUUID().toString

  val version: Int = 1

  var transformers: Seq[EntityTransformer[_, _]] = Seq()

object Entity:
  type Key[E <: Entity[_]] = E match
    case Entity[key] => key