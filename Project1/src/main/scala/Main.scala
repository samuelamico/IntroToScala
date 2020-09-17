import scala.io.StdIn._
import scala.annotation.tailrec

object Main extends App {

  // Exercises:
  def matchtype[T](variable: T): String = variable match{
    case _: String => "String"
    case _: Int => "Int"
    case _ => "Unknown"
  }

  // E1.
  def exercise1(line:String) = {
    def recursiveRead(line: String, max: Int = 0, min: Int = 0): Unit = line match  {
      case "quit" => print(s"Max = $max , Min = $min")
      case line if( matchtype(line) == "Unknown") => println("Unknown data type")
      case line if(line.toInt >= max) => recursiveRead(readLine(),line.toInt,min)
      case line if(line.toInt <= min) => recursiveRead(readLine(),max,line.toInt)
      case line => recursiveRead(readLine(),max,min)
    }
    if(line == "quit" || matchtype(line) == "Unknown") println("Not start")
    else recursiveRead(line,line.toInt,line.toInt)
  }

  //resposta E1 ---> exercise1(readLine())

  //
  // E2.
  def exercise2(value: Int,exp: Int): Int = {
    @tailrec
    def exponential(value: Int,exp: Int): Int = exp match {
      case 1 => value
      case _ => exponential(value*value,exp-1)
    }
    if(value < 0) throw new IllegalArgumentException
    else exponential(value,exp)
  }

  //resposta E1 ---> println(s"4¨2 =  ${exercise2(4,2)}")


}
