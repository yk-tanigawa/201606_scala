package org.stairwaybook.layout
import Element.elem

abstract class Element {
  def contents : Array[String]

  def width : Int =
    if (height == 0) 0 else contents(0).length
  def height : Int = contents.length

  def above(that: Element) = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }
  def beside(that: Element) = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem(
      for (
        (line1, line2) <- this1.contents zip that1.contents
      ) yield line1 + line2
    )
  }

  private def widen(w: Int): Element =
    if(w <= width) this
    else {
      val left  = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }
  private def heighten(h: Int): Element =
    if(h <= height) this
    else {
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot
    }

  override def toString = contents mkString "\n"

}

object Element {
  private class ArrayElement(
    val contents : Array[String]
  ) extends Element

  private class UniformElement(
    ch: Char,
    override val width: Int,
    override val height: Int
    ) extends Element {
    private val line = ch.toString * width
    def contents = Array.fill(height)(line)
  }

  private class LineElement(s: String) extends Element {
    val contents = Array(s)
    override def width = s.length
    override val height = 1
  }

  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)
  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)
  def elem(line: String): Element =
    new LineElement(line)

  def main(args: Array[String]) {
    val e1 = elem(Array("hello", "world"))
    val e2 = elem("hello")
    val e3 = elem("world!")
    val e4 = elem(Array("one", "two"))
    val e5 = elem("one")
    println(e2 above e3)
    println(e4 beside e5)
  }

}
