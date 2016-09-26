/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import scala.collection.mutable.ListBuffer

case class Model(rows: Int, columns: Int, cells: Array[Array[Cell]]) {

  private val NOT_VISITED = 0
  private val BEING_VISITED = 1
  private val DONE_VISITED = 2

  /**
   * Evaluation of the spreadsheet is done by initially
   * sorting the cells based on their ancestors. Later
   * going through each cell one by one in the sorted list
   * and evaluating their individual formulae. Evaluation
   * can detect cycles in input. When a cycle is detected
   * the Model raises the RuntimetimeException. 
   * 
   * @return
   */
  def evaluate: Array[String] = {
    implicit val modelCells = cells
    for (cell <- sortCells) {
      cell.evaluate
    }

    (for {
      row <- cells
      cell <- row
    } yield cell.evaluate.formatted("%.5f"))

  }

  def sortCells: List[Cell] = sortCellsTopologically

  /**
   * To evaluate the start position for all the cells,
   * initially the cells are sorted using topological
   * sort. After the sort the edges are arranged in
   * such a way that result[i-1] can be independently
   * evaluated without any references to result[i]
   *
   * @return
   */
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
      for ((predecessorRow, predecessorCol) <- vertex.predecessors) {
          val cellToVisit: Cell = cells(predecessorRow)(predecessorCol)
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
