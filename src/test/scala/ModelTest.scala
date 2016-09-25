/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import org.scalatest.{Matchers, FlatSpec}

class ModelTest extends FlatSpec with Matchers {
  "Model" should "order the cells according to dependencies" in {
    val model = Model(2, 3, Array(
      Array(
        Cell("A1", "A2"),
        Cell("A2", "5"),
        Cell("A3", "A1")),
      Array(
        Cell("B1", "A1 B2"),
        Cell("B2", "3"),
        Cell("B3", "B1 B2"))
    )
    )
    model.sortCells.map(x => x.name) should be(List("A2", "A1", "A3", "B2", "B1", "B3"))
  }

  it should "evaluate cells according to sort order" in {
    val model = Model(1, 2, Array(
      Array(
        Cell("A1", "A2"),
        Cell("A2", "5")
      )
    ))

    model.evaluate should be(Array("5.00000", "5.00000"))
  }

  it should "evaluate to Infinity when dividing by zero" in {
    val model = Model(1, 2, Array(
      Array(
        Cell("A1", "A2 0 /"),
        Cell("A2", "5")
      )
    ))

    model.evaluate should be(Array("Infinity", "5.00000"))
  }

  it should "throw has cycle exception when model detects cycle" in {
    val model = Model(1, 5, Array(
      Array(
        Cell("A1", "A2 ++"),
        Cell("A2", "A3 A4 +"),
        Cell("A3", "A4 ++"),
        Cell("A4", "A5 3 *"),
        Cell("A5", "A1 --")
      )
    ))

    val thrown = intercept[RuntimeException] {
      model.evaluate
    }

    thrown.getMessage should be("Cycle detected")
  }

  "Sample text case model" should "evaluate cells" in {
    val model = Model(2, 3, Array(
      Array(
        Cell("A1", "A2"),
        Cell("A2", "4 5 *"),
        Cell("A3", "A1")),
      Array(
        Cell("B1", "A1 B2 / 2 +"),
        Cell("B2", "3"),
        Cell("B3", "39 B1 B2 * /"))
    )
    )
    model.evaluate should be(Array("20.00000", "20.00000", "20.00000", "8.66667", "3.00000", "1.50000"))
  }

  "Example Model" should "evaluate cells with negative numbers, increments and decrements" in {
    val model = Model(4, 5, Array(
      Array(
        Cell("A1", "0"),
        Cell("A2", "D1 --"),
        Cell("A3", "A4 A5 *"),
        Cell("A4", "10"),
        Cell("A5", "20")),
      Array(
        Cell("B1", "C1"),
        Cell("B2", "A4"),
        Cell("B3", "10"),
        Cell("B4", "A1"),
        Cell("B5", "A1 A2 +")
      ),
      Array(
        Cell("C1", "A1 A2 +"),
        Cell("C2", "35"),
        Cell("C3", "B4 B5 -"),
        Cell("C4", "B3 A2 /"),
        Cell("C5", "C1 ++")
      ),
      Array(
        Cell("D1", "6"),
        Cell("D2", "-35"),
        Cell("D3", "D2 1 +"),
        Cell("D4", "D2 D3 +"),
        Cell("D5", "D1 D2 *")
      )
    )
    )

    model.evaluate should be(Array(
      "0.00000", "5.00000", "200.00000", "10.00000", "20.00000",
      "5.00000", "10.00000", "10.00000", "0.00000", "5.00000",
      "5.00000", "35.00000", "-5.00000", "2.00000", "6.00000",
      "6.00000", "-35.00000", "-34.00000", "-69.00000", "-210.00000"
    ))
  }
}
