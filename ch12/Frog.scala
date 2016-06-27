trait Philosophical {
  def philosophize() {
    println("I consume memory, therefore I am!")
  }
}

class Frog extends Philosophical {
  override def toString = "green"
}

object Frog {
  def main(args : Array[String]) {
    val f = new Frog
    println(f)
    f.philosophize()
  }
}
