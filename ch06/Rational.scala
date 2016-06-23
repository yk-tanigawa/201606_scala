import scala.language.implicitConversions

class Rational(n: Int, d: Int){
  require(d != 0)
  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n, 1)

  override def toString = numer + "/" + denom

  def +(that: Rational): Rational = 
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def +(i: Int): Rational =
    new Rational(numer + i * denom, denom)

  def -(that: Rational): Rational = 
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )

  def -(i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def *(that: Rational): Rational = 
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def *(i: Int): Rational =
    new Rational(numer * i, denom)

  def /(that: Rational): Rational = 
    new Rational(
      numer * that.denom,
      denom * that.numer
    )

  def /(i: Int): Rational =
    new Rational(numer, denom * i)


  def lessThan(that: Rational) = 
    numer * that.denom < numer * this.denom

  def max(that: Rational) = 
    if(lessThan(that)) that else this


  private def gcd(a: Int, b: Int): Int = 
    if(b == 0) a else gcd(b, a % b)

}

object Rational extends App {
  implicit def intToRational(x:Int) = new Rational(x)

  val x = new Rational(1, 2)
  val y = new Rational(2, 3)
  println(x)
  println(y)
  println(x + y)
  println(x * y)
  println(x + x * y)
  println(y * y)
  println(y * 2)
  println(2 * y)
}
