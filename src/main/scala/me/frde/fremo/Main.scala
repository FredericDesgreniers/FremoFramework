package me.frde.fremo

import me.frde.fremo.Fremo
import Fremo.givenSurface
import scala.quoted._
import scala.quoted.staging.{withQuotes, run, Compiler}

import scala.util.Random

object Main:
  given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit =
    println(withQuotes(test()))
    println(run(test()).apply(Array(1,2,3)))

  def test()(using Quotes) =
    val f: Expr[Array[Int] => Int] = {
      val stagedSum: Expr[Array[Int] => Int] =
        '{ (arr: Array[Int]) => ${('arr)}.sum}
      println(stagedSum)
      stagedSum
    }
    f


//  def main(args: Array[String]): Unit =
//    try
//      println(Fremo.currentScenario)
//      givenSurface(BitemporalSurface.now()) {
//        val name = "Fred " + new Random(System.currentTimeMillis()).nextInt()
//        val test = User(name)
//
//        Thread.sleep(1000)
//        val entity = givenSurface(BitemporalSurface.now()) {
//          DB.put(test)
//          DB.get[User](name)
//        }
//        println(s"$entity is loaded with time later than ${DB.get[User](name)}")
//
//      }
//    finally
//      CosmoDB.close