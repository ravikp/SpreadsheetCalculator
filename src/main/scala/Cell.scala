/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

case class Cell(name: String, formulae: String) {

  lazy val predecessors: List[(Int, Int)] = {
    val pattern = "([A-Z][0-9]+)".r
    pattern.findAllIn(formulae).matchData.map(x => x.group(1)).map(toRowColumnPair).toList
  }

  lazy val (row, col) = toRowColumnPair(name)

  lazy val indegree: Int = {
    predecessors.size
  }

  /**
   * Cell requires external model for evaluating its ancestors.
   * Cell can handle negative numbers in formulae. Formulae is
   * provided in RPN notation. Once evaluated its value cannot
   * be changed. It is further optimized by caching its computed
   * result.
   *
   * Supported operators are +, -, /, *, ++, --
   *
   * @param model
   * @return
   */
  def evaluate(implicit model: Array[Array[Cell]]): Double = {
    _value match {
      case Some(value) => value
      case None => {
        val result = evaluateValue
        _value = Some(result)
        result
      }
    }
  }

  private def evaluateValue(implicit model: Array[Array[Cell]]): Double = {

    val tokens = formulae.split(" ")
    import scala.collection.mutable.Stack
    val stack: Stack[Double] = new Stack[Double]
    for (token <- tokens) {
      if (isOperator(token)) token match {
        case "+" => {
          stack.push(stack.pop + stack.pop)
        }
        case "-" => {
          val x = stack.pop
          stack.push(stack.pop - x)
        }
        case "*" => {
          stack.push(stack.pop * stack.pop)
        }
        case "/" => {
          val x = stack.pop
          stack.push(stack.pop / x)
        }
        case "++" => {
          val x = stack.pop
          stack.push(x + 1)
        }
        case "--" => {
          val x = stack.pop
          stack.push(x - 1)
        }
      } else if (isCell(token)) {
          val (drow, dcol) = toRowColumnPair(token)
          stack.push(model(drow)(dcol).evaluate)
      }
      else
        stack.push(token.toDouble)
    }

    stack.pop
  }

  private def isOperator(token: String): Boolean = {
    token match {
      case "+" => true;
      case "-" => true;
      case "*" => true;
      case "/" => true;
      case "++" => true;
      case "--" => true;
      case _ => false
    }
  }

  private def isCell(token: String): Boolean = {
    val row = token.head
    'A' <= row && row <= 'Z'
  }

  private def toRowColumnPair(cellId: String): (Int, Int) = {
    (cellId.head - 'A', cellId.tail.toInt - 1)
  }

  private var _value: Option[Double] = None

  import scala.collection.mutable.ListBuffer
  private var _neighbours: ListBuffer[Cell] = ListBuffer.empty[Cell]

  def addNeighbour(cell: Cell):Unit = {
    _neighbours += cell
  }

  def neighbours = _neighbours.toList

  def hasNeighbours: Boolean = _neighbours.length > 0

}
