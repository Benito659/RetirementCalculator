package RetirementCalculationTest

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import org.RetirementCalculation.{RetCalcError, SimulatePlanApp}
import org.junit.Test
import org.scalatest.{Assertions, EitherValues}

class SimulatePlanIt extends Assertions with EitherValues {
  @Test def simulateARetirementPlanUsingMarketReturn(): Unit = {
    val actualResult = SimulatePlanApp.strMain(
      Array("1952.09,2017.09", "25", "40", "3000", "2000", "10000")
    )
    val expectedResult =
      s"""
         |Capital after 25 years of savings: 2958842
         |Capital after 40 years in retirement: 468925""".stripMargin

    assert(actualResult === Valid(expectedResult))

  }

  @Test def shouldReturnAnErrorWhenThePeriodsExceedTheReturnBounds(): Unit = {
    val actualResult = SimulatePlanApp.strMain(
      Array("1952.09,2017.09", "25", "60", "3000", "2000", "10000")
    )

    val expectedResult = "Cannot get the return for 780. " +
      "Accept range : 0 to 779"

    assert(actualResult == Invalid(expectedResult))
  }

  @Test def shouldReturnAnUsageExampleWhenTheNumberOfArgumentIsIncorrect(): Unit = {
    val result = SimulatePlanApp.strMain(
      Array("1952.09:2017.09", "25.0", "60", "3'000", "2000.0")
    )
    assert(result == Invalid(
      """Usage:
        |simulatePlan from,until nbOfYearsSaving nbOfYearsRetired netIncome currentExpenses initialCapital
        |Example:
        |simulatePlan 1952.09,2017.09 25 40 3000 2000 10000
        |""".stripMargin))
  }

  @Test def returnSeveralErrorWhenSeveralArgumentAreInvalid(): Unit = {
    val result = SimulatePlanApp.strMain(
      Array("1952.09:2017.09", "25.0", "60", "3'000", "2000.0", "10000"))
    println("dep",result.toString,"dep")
    println("dep", Invalid(
      """Invalid format for fromUntil. Expected :from,until,actual: 1952.09:2017.09
        |Invalid number for numberOfYearSaving : 25.0
        |Invalid number for netIncome : 3'000
        |Invalid number for currentExpenses : 2000.0""".stripMargin).toString,"dep")
    assert(result.toString == Invalid(
      """Invalid format for fromUntil. Expected :from,until,actual: 1952.09:2017.09 Invalid number for numberOfYearSaving : 25.0 Invalid number for netIncome : 3'000 Invalid number for currentExpenses : 2000.0""").toString)
  }

  @Test def shouldParseInt(): Unit = {
    val result = SimulatePlanApp.parseInt("test_parser", "3")
    val expected=Valid(3)
    println(result)
    assert(result == expected)
  }
  @Test def shouldNotParseInt(): Unit = {
    val result = SimulatePlanApp.parseInt("test_parser", "cool")
    val expected = Invalid(NonEmptyList.of(RetCalcError.InvalidNumber("test_parser", "cool")))
    assert(result==expected)
  }
  @Test def shouldParseFromUntil(): Unit = {
    //println(SimulatePlanApp.parseFromUntil())
  }

}
