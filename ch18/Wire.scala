

class Wire {
  def inverter(input: Wire, output: Wire)
  def andGate(a1: Wire, a2: Wire, output: Wire)
  def orGate(o1: Wire, o2: Wire, output: Wire)
  def halfAdder(a: Wire, b:Wire, s:Wire, c: Wire) {
    val d, e = new Wire
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }
  def fullAdder(
    a: Wire, b: Wire, cin: Wire,
    sum: Wire, cout: Wire) {
    val s c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }

}

