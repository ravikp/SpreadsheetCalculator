/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import scala.collection.mutable._

case class Model(rows: Int, columns: Int, cells: Array[Array[Cell]]) {

  lazy val flattenedCells = cells.flatten

  /**
   * This helps me to navigate via adjacency matrix
   */
  def buildNeighbours = {
    for (cell <- flattenedCells) {
      val dependencies = cell.predecessors
      for ((row, col) <- dependencies) {
        cells(row)(col).addNeighbour(cell)
      }
    }
  }

  buildNeighbours

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
    val result = Array.ofDim[String](rows * columns)
    sortCells.foreach(cell => {
      result(cell.row * columns + cell.col) = cell.evaluate.formatted("%.5f")
    })
    result
  }

  /**
   *
   * This topological sorting is based on Kahn's algorithm.
   * Kahn's algorithm doesn't depend on recursion. It depends
   * on maintaining two containers (one queue and another one
   * for the output)
   *
   * @return
   */
  def sortCells: Array[Cell] = {

    val indegree = Array.tabulate(rows, columns)((row, col) => cells(row)(col).indegree)

    var nodeVisitCount: Int = 0

    val cellsWithNoInbounds = flattenedCells.filter(_.indegree == 0)
    val queue = cellsWithNoInbounds.foldLeft(new Queue[Cell]())((q, cell) => {
      q.enqueue(cell);
      q
    })

    val result = ListBuffer[Cell]()

    var count = 0
    while (queue.nonEmpty) {
      val item = queue.dequeue

      result += item

      item.neighbours.foreach(cell => {
        indegree(cell.row)(cell.col) -= 1
        val result = indegree(cell.row)(cell.col)
        if (result == 0) {
          queue.enqueue(cell)
        }
      })

      count += 1
    }

    if (count != (rows * columns))
      throw new RuntimeException("Cycle detected")

    result.toArray
  }
}
