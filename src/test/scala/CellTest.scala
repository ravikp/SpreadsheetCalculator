/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import org.scalatest.{FlatSpec, Matchers}

class CellTest extends FlatSpec with Matchers {

  "Cell" should "evaluate cell formulae" in {
    val cell = Cell("A1", "2 3 +")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluateValue should be(5)
  }

  it should "evaluate cell formulae for negative numbers" in {
    val cell = Cell("A1", "2 -3 +")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluateValue should be(-1)
  }

  it should "evaluate cell formulae for increment" in {
    val cell = Cell("A1", "2 -3 + ++")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluateValue should be(0)
  }

  it should "evaluate cell formulae for decrement" in {
    val cell = Cell("A1", "2 -3 + --")
    implicit val model: Array[Array[Cell]] = Array(Array(cell))
    cell.evaluateValue should be(-2)
  }

  it should "evaluate cell formulae for 1 row with 2 cells" in {
    val cell1 = Cell("A1", "A2 20 *")
    val cell2 = Cell("A2", "2 3 +")
    implicit val model: Array[Array[Cell]] = Array(Array(cell1, cell2))
    cell1.evaluateValue should be(100.0d)
  }
}
