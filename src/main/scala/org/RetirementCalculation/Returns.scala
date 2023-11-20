package org.RetirementCalculation

sealed trait Returns {
  def monthlyRate(returns: Returns, month: Int) : Either[RetCalcError,Double] = returns match {

    case FixedReturns(r)=>Right(r/12)
    case VariableReturns(rs)=>
      if (rs.isDefinedAt(month)) {
        Right(rs(month).monthlyRate)
      } else
        Left(RetCalcError.ReturnMonthOutOfBounds(month,rs.size-1))

    case OffsetReturns(rs,offset)=>monthlyRate(rs,month+offset)
  }

  def fromEquityAndInflationData(equities: Vector[EquityData], inflations: Vector[InflationData]): VariableReturns = {
    VariableReturns(equities.zip(inflations).sliding(2).collect{
      case (prevEquity,prevInflation) +:(equity,inflation)+:Vector() =>
        val inflationRate = inflation.value/prevInflation.value
        val totalReturn = (equity.value+equity.monthlyDividend)/prevEquity.value
        val realTotalReturn = totalReturn-inflationRate
        VariableReturn(equity.monthId,realTotalReturn)
    }.toVector)
  }

}

case class DefaultReturns() extends Returns

case class FixedReturns(annualRate: Double) extends Returns
case class VariableReturns(returns: Vector[VariableReturn]) extends Returns {
  def fromUntil(monthsIdFrom: String, monthsIdUntil: String):VariableReturns = VariableReturns(returns.dropWhile(_.monthId !=monthsIdFrom).takeWhile(_.monthId!=monthsIdUntil))

}
case class OffsetReturns(orig: Returns, offset: Int) extends Returns

case class VariableReturn(monthId: String, monthlyRate: Double)

