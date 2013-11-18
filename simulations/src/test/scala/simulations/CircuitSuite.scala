package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(true)
    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or2 1")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "or2 2")

    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or2 3")
  }

  test("demux example 0") {
    val in, out = new Wire
    demux(in, List(), List(out))
    in.setSignal(true)
    run
    assert(out.getSignal === true, "demux 0.1")
    in.setSignal(false)
    run
    assert(out.getSignal === false, "demux 0.2")
  }

  def nWires(n: Int): List[Wire] = (for (j <- 0 until n) yield new Wire).toList
  def onlyNTrue(outs: List[Wire], n: Int): Boolean = {
    val outArray = outs.reverse.toArray
    (0 until outArray.length).forall((j) => (j != n && !outArray(j).getSignal) || (j == n && outArray(j).getSignal))
  }
  def noneTrue(outs: List[Wire]): Boolean = outs.forall(!_.getSignal)

  test("onlyNTrue") {
    val wires = nWires(5)
    assert((0 until 5).forall((i) => !onlyNTrue(wires, i)), "onlyNTrue 1")
    wires(1).setSignal(true)
    assert((0 until 3).forall((i) => !onlyNTrue(wires, i)), "onlyNTrue 2.1")
    assert(onlyNTrue(wires, 3), "onlyNTrue 2.2")
    assert(!onlyNTrue(wires, 4), "onlyNTrue 2.3")
  }

  test("demux example 1") {
    val in = new Wire
    val con = nWires(1)
    val outs = nWires(1 << 1)
    demux(in, con, outs)
    in.setSignal(true)
    con(0).setSignal(true)
    run
    assert(onlyNTrue(outs, 1), "demux 1.1")
    con(0).setSignal(false)
    run
    assert(onlyNTrue(outs, 0), "demux 1.2")
    in.setSignal(false)
    run
    assert(noneTrue(outs), "demux 1.3")
  }

  test("demux example 3") {
    val in = new Wire
    val con = nWires(3)
    val outs = nWires(1 << 3)
    demux(in, con, outs)
    in.setSignal(true)
    con(1).setSignal(true)
    run
    assert(onlyNTrue(outs, 2), "demux 3.1")
    con(0).setSignal(true)
    run
    assert(onlyNTrue(outs, 6), "demux 3.2")
    in.setSignal(false)
    run
    assert(noneTrue(outs), "demux 3.3")
  }
}
