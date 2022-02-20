package me.frde.fremo

import javax.script.ScriptEngineManager
import dotty.tools.repl._

object FremoRepl:
  def main(args: Array[String]): Unit = {
    val driver = new ReplDriver(args ++ Array("-usejavacp"))
    val state = driver.run("import me.frde.fremo._")(driver.initialState)
    driver.runUntilQuit(state)
  }