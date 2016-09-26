/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import org.scalatest.{FlatSpec, Matchers}

class CellTest extends FlatSpec with Matchers {

  "Cell" should "evaluate cell formulae" in {
    val cell = Cell("A1", "2 3 +")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluate should be(5)
  }

  it should "evaluate cell formulae for negative numbers" in {
    val cell = Cell("A1", "2 -3 +")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluate should be(-1)
  }

  it should "evaluate cell formulae for increment" in {
    val cell = Cell("A1", "2 -3 + ++")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluate should be(0)
  }

  it should "evaluate cell formulae for decrement" in {
    val cell = Cell("A1", "2 -3 + --")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluate should be(-2)
  }

  it should "evaluate cell formulae for 1 row with 2 cells" in {
    val cell1 = Cell("A1", "A2 20 *")
    val cell2 = Cell("A2", "2 3 +")
    implicit val model: Array[Array[Cell]] = Array(Array(cell1, cell2))
    cell1.evaluate should be(100.0d)
  }

  it should "increment cell formulae for MAX_INT" in {
    val cell = Cell("A1", s"${Integer.MAX_VALUE} ++")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluate should be(2147483648d)
  }

  it should "decrement cell formulae for MIN_INT" in {
    val cell = Cell("A1", s"${Integer.MIN_VALUE} --")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluate should be(-2147483649d)
  }
}
