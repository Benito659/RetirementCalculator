package org.RetirementCalculation

import scala.annotation.tailrec

import org.RetirementCalculation.RetCalcError.MoreExpensesThanIncome
case class RetCalc()
object RetCalc {

  def numberOfMonthSaving(interestRateReturns: Returns, numberOfMonthInRetirement: Int, netIncome: Int, currentExpense: Int, initialCapital: Int):Either[RetCalcError,Int] = {
    val params = RetCalcParams(
      numberOfMonthInRetirement = numberOfMonthInRetirement,
      netIncome = netIncome,
      currentExpense = currentExpense,
      initialCapital = initialCapital
    )
    @tailrec
    def loop(month: Int): Either[RetCalcError,Int] = {
      simulatePlan(
        returns = interestRateReturns,
        numberOfMonthSaving = month,
        params=params
      ) match {
        case Right((capitalAtRetirement,capitalAfterDeath))=>
          if(capitalAfterDeath>0.0)
            Right(month)
          else {
            loop(month+1)
          }
        case Left(err) => Left(err)
      }

    }
    if (netIncome>currentExpense)
      loop(0)
    else
      Left(MoreExpensesThanIncome(netIncome,currentExpense))
  }



  def futureCapital(returns: Returns, numberOfMonths: Int, netIncome: Int, currentExpense: Int,
                    initialCapital: Double): Either[RetCalcError,Double] = {
    val monthlySaving=netIncome-currentExpense
    (0 until numberOfMonths).foldLeft[Either[RetCalcError,Double]](Right(initialCapital)){
      case (accumulated,month)=>
        for {
          acc <- accumulated
          monthlyRate<-returns.monthlyRate(returns, month)
        } yield (acc * (1+monthlyRate) ) + monthlySaving
    }
  }

  def simulatePlan(returns: Returns,params: RetCalcParams, numberOfMonthSaving: Int,monthOffSet:Int=0): Either[RetCalcError,(Double, Double)] = {
    import params._
    for{
      capitalAtRetirement <- futureCapital(
        returns = OffsetReturns(returns,monthOffSet),
        numberOfMonths = numberOfMonthSaving,
        netIncome = netIncome,
        currentExpense = currentExpense,
        initialCapital = initialCapital
      )
      capitalAfterDeath <- futureCapital(
        returns = OffsetReturns(returns, monthOffSet+numberOfMonthSaving),
        numberOfMonths = numberOfMonthInRetirement,
        netIncome = 0,
        currentExpense = currentExpense,
        initialCapital = capitalAtRetirement
      )
    } yield (capitalAtRetirement,capitalAfterDeath)
  }

}

case class RetCalcParams(numberOfMonthInRetirement: Int, netIncome: Int, currentExpense: Int, initialCapital: Int)
