/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */
trait CanBuildDataInputReader extends InputDataReader {
  def lines: List[String]

  def readModel: Model = {
    val dimensions :: cellData = lines
    val (columns, rows) = parseDimensions(dimensions)
    val cells = parseCells(rows, columns, cellData)
    Model(rows, columns, cells)
  }

  def parseDimensions(dimensions: String): (Int, Int) = {
    val values = dimensions.split(" ")
    (values(0).toInt, values(1).toInt)
  }

  def parseCells(rows: Int, columns: Int, cellData: List[String]): Array[Array[Cell]] = {

    val cellIdentifiers: List[String] = (for {
      rowid <- ('A' to 'Z').take(rows)
      colid <- (1 to columns)
    } yield (s"$rowid$colid")).toList

    val result = Array.ofDim[Cell](rows, columns)

    val cells: Array[Array[Cell]] = cellIdentifiers.zip(cellData).map {
      case (id, celldatum) => Cell(id, celldatum)
    }.foldLeft(result)((result, cell) => {
      result(cell.row)(cell.col) = cell;
      result
    })

    cells

  }
}
