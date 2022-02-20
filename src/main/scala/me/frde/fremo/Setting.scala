package me.frde.fremo

case class Setting(override val key: String, settingValue: String) extends Entity[String]
