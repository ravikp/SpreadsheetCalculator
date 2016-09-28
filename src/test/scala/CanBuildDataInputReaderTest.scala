/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import org.scalatest.{FlatSpec, Matchers}

class CanBuildDataInputReaderTest extends FlatSpec with Matchers {
  "CanBuildDataInputReader" should "create model with a single cell" in new CanBuildDataInputReader {
    val lines = List("1 1", "3")
    val cell1 = Cell("A1", "3")

    readModel.cells should be(Array(Array(cell1)))
  }

  it should "create model with a single row" in new CanBuildDataInputReader {
    val lines = List("2 1", "A2", "5")
    val cell1 = Cell("A1", "A2")
    val cell2 = Cell("A2", "5")

    readModel.cells should be(Array(Array(cell1, cell2)))
  }
}
