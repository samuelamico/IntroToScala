import org.scalatest.{FlatSpec, Matchers}
import Cap6.Cap6

class MainSpec extends FlatSpec with Matchers {

  // Chapter 6 Test
  it should "Return the exponetial for 4^2" in {
    Cap6.exercise2(4,5) shouldEqual(1024)
  }

  it should "Return the exponetial for 23^2" in {
    Cap6.exercise2(23,2) shouldEqual(529)
  }

  an [IllegalArgumentException] should be thrownBy Cap6.exercise2(-1,2)

  it should "Return no Prime" in {
    Cap6.exercise3(4) shouldEqual(false)
  }

  it should "Return Prime" in {
    Cap6.exercise3(7) shouldEqual(true)
  }



}

