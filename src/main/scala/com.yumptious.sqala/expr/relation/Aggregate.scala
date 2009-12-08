package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

class AggregateSelectExpr(
    val groupByColumns    : Seq[NamedColExpr[_,_ <: NullStatus]],
    columns               : Seq[NamedColExpr[_,_ <: NullStatus]],
    from                  : FromExpr,
    whereCond             : ColExpr[Boolean,MaybeNull],
    val havingCond        : ColExpr[Boolean,MaybeNull],
    orderBy               : Seq[OrderExpr]
  ) extends SelectExpr(columns, from, whereCond, orderBy) {

  override def where(condition : ColExpr[Boolean,MaybeNull]) : AggregateSelectExpr = {
    val newHaving = if (havingCond eq null) condition else new AndOp(havingCond, condition)
    new AggregateSelectExpr(groupByColumns, columns, from, whereCond, newHaving, orderBy)
  }
  def having(condition : ColExpr[Boolean,MaybeNull]) = where(condition)

  override def select(columns : NamedColExpr[_,_ <: NullStatus]*) : AggregateSelectExpr = {
    new AggregateSelectExpr(groupByColumns, columns, from, whereCond, havingCond, orderBy)
  }

  override def toSQL = {
    val havingSQL = if (havingCond eq null) "" else " HAVING " + havingCond.toSQL

    // if no 'group by' is specified we leave it out of the SQL, which means grouping by all columns.
    val groupBySQL = if (groupByColumns eq null) "" else " GROUP BY " + groupByColumns.map(_.nameSQL).mkString(", ")

    "SELECT " + columnsSQL + " FROM " + from.toSQL + whereSQL + groupBySQL + havingSQL + orderSQL
  }
}
