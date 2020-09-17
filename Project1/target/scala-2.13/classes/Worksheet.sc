def factorNumber(value: Int): List[Int] = {
  def from(n: Int): Stream[Int] = n #:: from(n+1)

  def sieve(s: Stream[Int]): Stream[Int] =
  {
    s.head #:: sieve(s.tail filter (_ % s.head != 0 ))
  }

  val primes = sieve(from(2))

  def  recursive(new_value: Int, lista: List[Int]): List[Int] = new_value match {
    case 1 => lista.reverse
    case n => {
      val factor: Int = primes.filter(new_value % _ == 0).take(1).toList.head
      recursive(new_value / factor,factor :: lista)
    }
  }
  recursive(value,Nil)
}

factorNumber(17)