package me.frde.fremo

import java.util.concurrent.{CompletableFuture, CompletionStage}

object Fremo {
  var currentScenario: ThreadLocal[Scenario] = ThreadLocal.withInitial(() => Scenario(None, BitemporalSurface.now(), ScenarioTweaks(Map())))

  def givenSurface[T](surface: BitemporalSurface)(body: => T): T = {
    val newScenario = Scenario(Some(currentScenario.get().copyScenario()), surface, ScenarioTweaks(Map()))

    currentScenario.set(newScenario)
    val result = body
    currentScenario.set(newScenario.parentScenario.get)
    result
  }

  def givenTweak[T, R, S](propertyName: String, propertyValue: R)(body: => S): S = {
    val result = body
    body
  }

  def spawn[T](inner: => T): CompletionStage[T] = {
    CompletableFuture.completedStage(inner)
  }

  def spawnThread(body: => Unit): Thread =
    val newThreadScenario = currentScenario.get().copyScenario()
    val thread = new Thread():
      override def run(): Unit =
        currentScenario.set(newThreadScenario)
        body

    thread.start()
    thread
}
