/**
 * Copyright (c) September 26, 2016. This code intended for discussion purpose.
 * All commercial use requires permission from the author (Ravi Kumar Pasumarthy: ravi.pasumarthy@gmail.com)
 */
class IOReader extends CanBuildDataInputReader {
  override def lines: List[String] = io.Source.stdin.getLines().toList
}
