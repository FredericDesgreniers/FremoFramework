package me.frde.fremo

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable.Buffer

object Reactive:
  def react(pipes: ReactivePipe[_]*) =
    new Reactor(pipes)

class Reactor(val pipes: Seq[ReactivePipe[_]]):
  def start() =
    val clonedPipes = pipes.map(_.copyPipe())
    clonedPipes.foreach(_.start())
    clonedPipes.map(_.source).distinct.foreach(_.start())
    RunningReactor(clonedPipes)


case class RunningReactor(pipes: Seq[ReactivePipe[_]]) extends AutoCloseable:
  override def close(): Unit = pipes.foreach(_.close())

case class ReactivePipe[T](source: Source[T], target: Target[T]) extends AutoCloseable:
  def start() =
    source.subscribe(this, target.react)

  def copyPipe() = ReactivePipe(source, target)

  override def close(): Unit = source.unsubscribe(this)

trait Source[T]:
  val pipes = Buffer[(Object, T => Unit)]()
  def subscribe(owner: Object, callback: T => Unit): Unit =
    synchronized(
      pipes.append((owner, callback))
    )

  def unsubscribe(owner: Object) =
    synchronized(
      pipes.remove(pipes.indexWhere(_._1 == owner))
    )

  def -->(target: Target[T]): ReactivePipe[T] =
    ReactivePipe[T](this, target)

  def publish(item: T) =
    Fremo.givenSurface(BitemporalSurface.now()) {
      pipes.foreach(pipe => {
        try
          pipe._2(item)
        catch
          case e: Exception => e.printStackTrace()
      }
      )
    }

  var started = new AtomicBoolean(false)

  def start(): Unit =
    if !started.getAndSet(true) then
      Fremo.spawnThread(startLoop())

  def startLoop(): Unit =
    ()

trait Target[T]:
  def <--(source: Source[T]): ReactivePipe[T] =
    ReactivePipe(source, this)

  def react(item: T): Unit

trait Transformer[I, O] extends Source[O] with Target[I]:
  def transform(input: I): O

  override def react(item: I): Unit = publish(transform(item))

  override def start(): Unit = ()
  override def startLoop(): Unit = ()

object Transformer {
  def apply[I, O](transformFn: I => O) =
    new Transformer[I, O] {
      override def transform(input: I): O = transformFn(input)
    }
}

implicit class TransformerLike[I, O](implicit fn: I => O) extends Transformer[I, O] {
  override def transform(input: I): O = fn(input)
}

object Main2:
  def main(args: Array[String]): Unit =
    val source = new Source[Int] {
      override def startLoop(): Unit =
        for(
          i <- 1 to 10
        ) yield {
          Thread.sleep(1000)
          publish(i)
        }
    }
    val target = new Target[String]:
      override def react(item: String): Unit = println(item)

    val transformer = Transformer((i: Int) => s"Got item ${i.toString}")

    val reactor = Reactive.react(
      source --> transformer,
      target <-- transformer
    ).start()