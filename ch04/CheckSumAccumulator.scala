import scala.collection.mutable.Map

class CheckSumAccumulator {
  private var sum = 0
  def add(b : Byte): Unit = sum += b
  def checksum(): Int = ~(sum & 0xFF) + 1
}

object CheckSumAccumulator {
  private var cache = Map[String, Int]()

  def calculate(s: String): Int =
    if(cache.contains(s))
      cache(s)
    else {
      val acc = new CheckSumAccumulator
      for(c <- s)
        acc.add(c.toByte)
      val cs = acc.checksum()
      cache += (s -> cs)
      cs
    }

  def main(args: Array[String]) {
    if(args.length > 0)
      println(calculate(args(0)))    
  }
}
