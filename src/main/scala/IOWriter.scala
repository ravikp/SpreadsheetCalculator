/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */
class IOWriter(rows: Int, columns: Int, cells: Array[String]){
  def print = {
    println(s"$columns $rows")
    for(entry <- cells){
      println(entry)
    }
  }
}
