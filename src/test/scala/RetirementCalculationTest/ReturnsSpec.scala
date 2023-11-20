package RetirementCalculationTest

import org.RetirementCalculation._
import org.junit.Test
import org.scalatest.{Assertions, EitherValues}
class ReturnsSpec extends Assertions with EitherValues {

  val variablesReturn = VariableReturns(Vector(VariableReturn("2000.01", 0.1), VariableReturn("2000.02", 0.2)))

  @Test def shouldReturnAFixedRateForAFixedReturn(): Unit = {
    assert(DefaultReturns().monthlyRate(FixedReturns(0.04), 0).right.value === (0.04 / 12))
    assert(DefaultReturns().monthlyRate(FixedReturns(0.04), 10).right.value === (0.04 / 12))
  }

  @Test def shouldReturnTheNthRateForVariableReturn(): Unit = {

    assert(DefaultReturns().monthlyRate(variablesReturn, 0).right.value === 0.1)
    assert(DefaultReturns().monthlyRate(variablesReturn, 1).right.value === 0.2)
  }


  @Test def shouldReturnNthOffsetRateForOffsetReturn(): Unit = {
    val returns = OffsetReturns(variablesReturn, 1)
    assert(DefaultReturns().monthlyRate(returns, 0).right.value === (0.2))
  }
  @Test def shouldReturnNoneIfNGreaterThanLength(): Unit = {
    assert(DefaultReturns().monthlyRate(variablesReturn,2).left.value ===RetCalcError.ReturnMonthOutOfBounds(2,1))
    assert(DefaultReturns().monthlyRate(variablesReturn,3).left.value ===RetCalcError.ReturnMonthOutOfBounds(3,1))
  }
  @Test def computeRealTotalReturnFromEquityAndInflationData(): Unit = {
    val equity=Vector(
      EquityData("2017.01",100.0,10.0),
      EquityData("2017.02",101.0,12.0),
      EquityData("2017.03",102.0,12.0)
    )
    val inflation=Vector(
      InflationData("2017.01",100.0),
      InflationData("2017.02",102.0),
      InflationData("2017.03",102.0)
    )

    val returns=DefaultReturns().fromEquityAndInflationData(
      equity,inflation
    )
    assert(returns === VariableReturns(
      Vector(
        VariableReturn("2017.02",((101.0+12.0/12)/100.0) - (102.0/100.0)),
        VariableReturn("2017.03",((102.0+12.0/12)/101.0) - (102.0/102.0))
        )
    ))
  }

}
