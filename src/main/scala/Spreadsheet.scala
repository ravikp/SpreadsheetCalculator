/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */

import scala.util.{Failure, Try, Success}

object Spreadsheet {
  def main(args: Array[String]): Unit = {
    val model = new IOReader().readModel

    Try(model.evaluate) match {
      case Success(_) => new IOWriter(model.rows, model.columns, model.evaluate).print
      case Failure(e) => System.exit(-1)
    }
  }
}

