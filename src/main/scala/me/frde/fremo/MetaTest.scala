package me.frde.fremo

import scala.Tuple.Concat

object MetaTest:
  def main(args: Array[String]): Unit =
    val thing = choose(true)
    thing.test()

  type *:[H, T <: Tuple] <: Tuple

  transparent inline def choose(b: Boolean): A =
    inline if (b) then
      new B
    else
      new A

class A

class B extends A:
  def test() = println("test")