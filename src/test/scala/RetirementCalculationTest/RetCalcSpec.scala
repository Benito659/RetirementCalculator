package RetirementCalculationTest

import org.RetirementCalculation.RetCalcError.MoreExpensesThanIncome
import org.RetirementCalculation._
import org.junit.Test
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.{Assertions, EitherValues}
class RetCalcSpec extends Assertions with EitherValues{
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.0001)

  val params = RetCalcParams(
    numberOfMonthInRetirement=40*12,
    netIncome=3000,
    currentExpense=2000,
    initialCapital= 10000
  )
  @Test def retCalcShouldCalculateAmountSavingsInNMonths() {
    val actual = RetCalc.futureCapital(
      FixedReturns(0.04), numberOfMonths = 25 * 12,
      netIncome = 3000, currentExpense = 2000,
      initialCapital = 10000
    ).right.value
    val expected = 541267.1990
    assert(actual === expected)
  }

  @Test def calculateHowMuchSavingShouldBeLeftAfterNMonths(): Unit = {
    val actual = RetCalc.futureCapital(
      FixedReturns(0.04 ), numberOfMonths = 40 * 12,
      netIncome = 0, currentExpense = 2000,
      initialCapital = 541267.1990
    ).right.value
    val expected = 309867.53176
    assert(actual === expected)
  }

  @Test def calculateTheCapitalAtTheRetirementAnAfterTheDeath(): Unit = {
    val capital = RetCalc.simulatePlan(
      returns = FixedReturns(0.04 ), params,numberOfMonthSaving = 25 * 12
    ).right.value
    val capitalAtRetirement = capital._2
    val capitalAfterDeath = capital._1
    assert(capitalAtRetirement === (309867.5316))
    assert(capitalAfterDeath === (541267.1990))
  }

  @Test def shouldUseDifferentReturnForCapitalisationAndDrawDown(): Unit = {
    val numberOfMonthsOfSaving=25*12
    val returns = VariableReturns(
      Vector.tabulate(numberOfMonthsOfSaving+params.numberOfMonthInRetirement)(
        i=>
          if(i<numberOfMonthsOfSaving)
            VariableReturn(i.toString,0.04/12)
          else
            VariableReturn(i.toString,0.03/12)
      )
    )
    val capital = RetCalc.simulatePlan(
      returns,params,numberOfMonthsOfSaving
    ).right.value
    val capitalAtRetirement = capital._1
    val capitalAfterDeath = capital._2
    assert(capitalAtRetirement ===(541267.1990))
    assert(capitalAfterDeath ===(-57737.7227))
  }

  @Test def calculateHowLongINeedToSaveBeforeICanRetire(): Unit = {
    val actual = RetCalc.numberOfMonthSaving(
      interestRateReturns = FixedReturns(0.04 ), numberOfMonthInRetirement = 40 * 12,
      netIncome = 3000, currentExpense = 2000, initialCapital = 10000
    ).right.value
    val expected = 23 * 12 + 1
    assert(actual === expected)
  }

  @Test def notCrashIfTheResultingNumberOfMonthIsVeryHigh(): Unit = {
    val actual = RetCalc.numberOfMonthSaving(
      interestRateReturns = FixedReturns(0.01 ), numberOfMonthInRetirement = 40 * 12,
      netIncome = 3000, currentExpense = 2999, initialCapital = 0).right.value
    val expected = 8280
    assert(actual === expected)
  }

  @Test def shouldNotLoopForeverIfIEnterBadParameter(): Unit = {
    val actual = RetCalc.numberOfMonthSaving(
      interestRateReturns = FixedReturns(0.04), numberOfMonthInRetirement = 40 * 12
      , netIncome = 1000, currentExpense = 2000, initialCapital = 10000
    ).left.value
    assert(actual === MoreExpensesThanIncome(1000,2000))
  }


}
