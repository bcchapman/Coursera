package simulation

abstract class Gates extends Simulation  {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire extends Simulation {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit =
      if(s != sigVal) {
        sigVal = s
        actions foreach (_())
      }

    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction() = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output setSignal !inputSig}
    }

    input addAction invertAction
  }

  def andGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    def orAction() = {
      val in1Sig = input1.getSignal
      val in2Sig = input2.getSignal
      afterDelay(AndGateDelay) { output setSignal (in1Sig & in2Sig)}
    }
    input1 addAction orAction
    input2 addAction orAction
  }

  def orGate(input1: Wire, input2: Wire, output: Wire): Unit = {
    def orAction() = {
      val in1Sig = input1.getSignal
      val in2Sig = input2.getSignal
      afterDelay(OrGateDelay) { output setSignal (in1Sig | in2Sig)}
    }
    input1 addAction orAction
    input2 addAction orAction
  }

  def orGate1(input1: Wire, input2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(input1, notIn1); inverter(input2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }
}
