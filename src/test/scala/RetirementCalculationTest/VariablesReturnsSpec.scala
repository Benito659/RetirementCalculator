package RetirementCalculationTest

import org.RetirementCalculation.{VariableReturn, VariableReturns}
import org.junit.Test
import org.scalatest.Assertions

class VariablesReturnsSpec extends Assertions {
  @Test def shouldKeepOnlyWindowsOfReturns(): Unit = {
    val  variableReturns=VariableReturns(
      Vector.tabulate(12){
        i => val d = (i+1).toDouble
          VariableReturn(f"2017.$d%02.0f",d)
      }
    )
    assert(variableReturns.fromUntil("2017.07","2017.09").returns === (Vector(VariableReturn("2017.07",7.0),VariableReturn("2017.08",8.0))))
    assert(variableReturns.fromUntil("2017.10","2018.01").returns === (Vector(VariableReturn("2017.10",10.0),VariableReturn("2017.11",11.0),VariableReturn("2017.12",12.0))))
  }
}
