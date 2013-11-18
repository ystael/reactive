package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def connect(input: Wire, output: Wire) {
    input.addAction(() => { output.setSignal(input.getSignal) })
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(o1: Wire, o2: Wire, output: Wire) {
    def orAction() {
      val o1Sig = o1.getSignal
      val o2Sig = o2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(o1Sig || o2Sig) }
    }
    o1.addAction(orAction)
    o2.addAction(orAction)
  }
  
  def orGate2(o1: Wire, o2: Wire, output: Wire) {
    val no1, no2, ao = new Wire
    inverter(o1, no1)
    inverter(o2, no2)
    andGate(no1, no2, ao)
    inverter(ao, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case List()    => out match {
        case o::Nil => connect(in, o)
        case _      => throw new IllegalArgumentException("demux with no control must have exactly one out")
      }
      case con::cons => {
        val outs1 = out.take(out.length / 2)
        val outs0 = out.drop(out.length / 2)
        val in1, in0, negcon = new Wire
        andGate(in, con, in1)
        inverter(con, negcon)
        andGate(in, negcon, in0)
        demux(in1, cons, outs1)
        demux(in0, cons, outs0)
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
