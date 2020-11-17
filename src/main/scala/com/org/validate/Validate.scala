package com.org.valid

object Validate {
  
  def validate(puzzle: Array[Array[Int]]): Boolean = {
    lazy val rowsValid = puzzle.forall(hasNoDuplicates)
    lazy val columnsValid = puzzle.transpose.forall(hasNoDuplicates)
    lazy val cellsValid = squaresHaveNoDuplicates(puzzle)

    rowsValid && columnsValid && cellsValid
  }

  private def hasNoDuplicates(line: Array[Int]) = line.distinct.count(1 to 9 contains _) == line.count(_ != 0)

  private def squaresHaveNoDuplicates(matrix: Array[Array[Int]]) = {
    val rowBlocks = matrix.grouped(3).toArray
    val transposed = rowBlocks.map(_.transpose)
    val squares = transposed.map(_.grouped(3).toArray)
    val squaresFlattened = squares.map(_.map(_.flatten))

    squaresFlattened.forall(_.forall(hasNoDuplicates))
  }
 def readCSV(filename : String) : Array[Array[Int]] = {
    val bufferedSource = io.Source.fromFile(filename)
    var matrix :Array[Array[Int]] = Array.empty
    for (line <- bufferedSource.getLines) {
        val cols = line.split(",").map(_.trim.toInt)
        matrix = matrix :+ cols
    }
    bufferedSource.close
    return matrix
 }

def main(args: Array[String]) 
 {
  if (args.length == 0) {
        println("need sudocku file path as parameter")
    }
  val filename = args(0)
    var sudoku :Array[Array[Int]] = readCSV(filename)
    if(validate(sudoku).booleanValue()== true){
          print("SUdoku is valid")
     }
      else print("SUdoku is invalid")
    }
 }