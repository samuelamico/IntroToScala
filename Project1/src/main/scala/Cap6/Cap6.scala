package Cap6

import scala.io.StdIn.readLine

object Cap6 extends chapter6Interface {

  // Exercises:
  def matchType[T](variable: T): String = variable match{
    case _: String => "String"
    case _: Int => "Int"
    case _ => "Unknown"
  }

  // E1.
  def exercise1(line:String) = {
    def recursiveRead(line: String, max: Int = 0, min: Int = 0): Unit = line match  {
      case "quit" => print(s"Max = $max , Min = $min")
      case line if( matchType(line) == "Unknown") => println("Unknown data type")
      case line if(line.toInt >= max) => recursiveRead(readLine(),line.toInt,min)
      case line if(line.toInt <= min) => recursiveRead(readLine(),max,line.toInt)
      case line => recursiveRead(readLine(),max,min)
    }
    if(line == "quit" || matchType(line) == "Unknown") println("Not start")
    else recursiveRead(line,line.toInt,line.toInt)
  }

  //
  // E2.
  def exercise2(value: Int,exp: Int): Int = {
    def exponential(value: Int,exp: Int, inital: Int): Int = exp match {
      case 1 => value
      case n if(n%2 == 0) => exponential(value*value,exp/2,inital)
      case n if(n%2 != 0) => inital * exponential(value*value,(exp-1)/2,inital)
    }
    if(value < 0) throw new IllegalArgumentException
    else if(value == 0) 1
    else exponential(value,exp,value)
  }



  //
  // E9.
  def exercise3(value:Int): Boolean = {
    val rangeList = List.range(1,value)
    rangeList.filter(x => value%x == 0).length == 1
  }


  //
  // E10.
  def factorNumber(value: Int): List[Int] = {
    ???
  }

  
}
