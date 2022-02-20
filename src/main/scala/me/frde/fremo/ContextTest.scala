package me.frde.fremo

import scala.collection.mutable.ArrayBuffer

class Table:
  val rows = new ArrayBuffer[Row]
  def add(row: Row) = rows += row
  override def toString: String = rows.mkString("Row(", ", ", ")")

class Row:
  val cells = new ArrayBuffer[Cell]
  def add(cell: Cell) = cells += cell
  override def toString: String = cells.mkString("Cell(", ", ", ")")

case class Cell(value: String)

object ContextTest {
  def table(init: Table ?=> Unit) =
    given t: Table = Table()
    init
    t

  def row(init: Row ?=> Unit) (using t: Table) =
    given r: Row = Row()
    init
    t.add(r)

  def cell(str: String)(using r: Row) =
    r.add(Cell(str))

  def main(args: Array[String]): Unit =
    println(table {
      row{
        cell("test")
        cell("abc")
      }
    })

}
