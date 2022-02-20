package me.frde.fremo

import scala.reflect.ClassTag

case class Scenario(parentScenario: Option[Scenario], surface: BitemporalSurface, tweaks: ScenarioTweaks) {
  def copyScenario(): Scenario =
    Scenario(parentScenario, surface, tweaks)

  def getTweak[T, R](propertyName: String)(implicit evidence: ClassTag[T]): R =
    tweaks.tweaks(evidence.runtimeClass)(propertyName).asInstanceOf[R]
}

case class ScenarioTweaks(tweaks: Map[Class[_], Map[String, _]])

case class ScenarioTweak[T](propertyName: String, newValue: T)
