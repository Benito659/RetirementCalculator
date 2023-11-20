package org.RetirementCalculation

import cats.data.{NonEmptyList, Validated}
import cats.implicits.{catsSyntaxTuple3Semigroupal, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedId}
import org.RetirementCalculation.RetCalcError.{InvalidArgument, InvalidNumber, RetCalcResult}
object SimulatePlanApp extends App {
  def parseInt(name:String,value: String):RetCalcResult[Int] = {
    Validated.catchOnly[NumberFormatException](value.toInt)
      .leftMap(_=>NonEmptyList.of(InvalidNumber(name,value)))
  }
  def parseFromUntil(fromUntil:String):RetCalcResult[(String,String)] = {
    val array = fromUntil.split(",")
    if(array.length!=2)
      InvalidArgument(name="fromUntil",value=fromUntil,expectedFormat="from,until").invalidNel
    else
      (array(0),array(1)).validNel
  }
  def parseParams(args:Array[String]): RetCalcResult[RetCalcParams]= {
    (
      parseInt("numberOfYearInRetirement",args(2)),
      parseInt("netIncome",args(3)),
      parseInt("currentExpenses",args(4)),
      parseInt("initialCapital",args(5))
    ).mapN{
      case (numberOfYearInRetirement,netIncome,currentExpenses,initialCapital)=>RetCalcParams(
        numberOfMonthInRetirement = numberOfYearInRetirement*12, netIncome = netIncome, currentExpense = currentExpenses, initialCapital = initialCapital
      )
    }
  }

  def strSimulatePlan(returns: Returns, numberOfYearsSaving:Int, params: RetCalcParams): RetCalcResult[String] = {
    val result = RetCalc
      .simulatePlan(returns=returns,params = params,numberOfMonthSaving = numberOfYearsSaving*12)
      .map{
        case (capitalAtRetirement,capitalAfterDeath) =>
          val numberOfYearInRetirement=params.numberOfMonthInRetirement/12
          s"""
             |Capital after $numberOfYearsSaving years of savings: ${capitalAfterDeath.round}
             |Capital after $numberOfYearInRetirement years in retirement: ${capitalAtRetirement.round}""".stripMargin

      }
    Validated.fromEither(result).toValidatedNel
  }

  println(strMain(args))
  def strMain(args: Array[String]): Validated[String,String] = {
    if(args.length!=6)
      """Usage:
        |simulatePlan from,until nbOfYearsSaving nbOfYearsRetired netIncome currentExpenses initialCapital
        |Example:
        |simulatePlan 1952.09,2017.09 25 40 3000 2000 10000
        |""".stripMargin.invalid
    else {
      val allReturns = DefaultReturns().fromEquityAndInflationData(
        equities = EquityData.fromResource("sp500.tsv"),
        inflations = InflationData.fromResource("cpi.tsv")
      )
      val vFromUntil = parseFromUntil(args(0))
      val vNumberOfYearSaving=parseInt("numberOfYearSaving",args(1))
      val vParams=parseParams(args)
      (vFromUntil, vNumberOfYearSaving, vParams)
        .tupled
        .andThen { case ((from, until), nbOfYearsSaving, params) =>
          strSimulatePlan(allReturns.fromUntil(from, until),
            nbOfYearsSaving, params)
        }
        .leftMap(nel => nel.map(_.message).toList.mkString(" "))
    }
  }
}
