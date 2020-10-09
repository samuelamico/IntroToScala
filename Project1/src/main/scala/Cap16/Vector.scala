package Cap16

/*
    Create a vector class that are mutable, defines this methods:
    a) +=,-=,*=,/=
    b) keep the rest of methods the same
 */

class Vector(var x:Int, var y: Int, var z: Int) {

  def apply(i:Int):Int = i match{
    case 0 => this.x
    case 1 => this.y
    case 2 => this.z
  }

  def update(pos:Int,i:Int) = pos match {
    case 0 => this.x = i
    case 1 => this.y = i
    case 2 => this.z = i
  }

  def +=(v:Vector) = {
      this.x = this.x + v.x
      this.y = this.y + v.y
      this.z = this.z + v.z
  }

  override def toString() = "<"+x+", "+y+", "+z+">"
}
