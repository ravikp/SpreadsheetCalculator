/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import scala.collection.mutable.ListBuffer

case class Model(rows: Int, columns: Int, cells: Array[Array[Cell]]) {

  private val NOT_VISITED = 0
  private val BEING_VISITED = 1
  private val DONE_VISITED = 2

  def evaluate: Array[String] = {
    implicit val modelCells = cells
    for (cell <- sortCells) {
      cell.evaluateValue
    }

    (for {
      row <- cells
      cell <- row
    } yield cell.evaluateValue.formatted("%.5f"))

  }

  def sortCells: List[Cell] = sortCellsTopologically

  private def sortCellsTopologically: List[Cell] = {

    val out = ListBuffer[Cell]()
    val visitedVertices = Array.ofDim[Int](rows, columns)

    val sortedByEvaluation = new ListBuffer[Cell]()

    def visitVertex(vertex: Cell): Unit = {

      if(visitedVertices(vertex.row)(vertex.col) == BEING_VISITED)
        throw new RuntimeException("Cycle detected")

      if(visitedVertices(vertex.row)(vertex.col) == DONE_VISITED)
        return;

      visitedVertices(vertex.row)(vertex.col) = BEING_VISITED

      for ((neighbourRow, neighbourCol) <- vertex.neighbours) {
          val cellToVisit: Cell = cells(neighbourRow)(neighbourCol)
          visitVertex(cellToVisit)
      }

      visitedVertices(vertex.row)(vertex.col) = DONE_VISITED
      sortedByEvaluation.+=(vertex)
    }

    for (vertex <- cells.flatten) {
      if (visitedVertices(vertex.row)(vertex.col) == NOT_VISITED) visitVertex(vertex)
    }

    sortedByEvaluation.toList

  }
}
