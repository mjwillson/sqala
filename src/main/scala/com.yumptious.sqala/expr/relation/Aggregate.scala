package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

class AggregateSelectExpr(
    val groupByColumns    : Seq[NamedColExpr[_]],
    columns               : Seq[NamedColExpr[_]],
    from                  : FromExpr,
    whereCond             : ColExpr[Boolean],
    val havingCond        : ColExpr[Boolean],
    orderBy               : Seq[OrderExpr]
  ) extends SelectExpr(columns, from, whereCond, orderBy) {

  override def where(condition : ColExpr[Boolean]) : AggregateSelectExpr = {
    val newHaving = if (havingCond eq null) condition else new AndOp(havingCond, condition)
    new AggregateSelectExpr(groupByColumns, columns, from, whereCond, newHaving, orderBy)
  }
  def having(condition : ColExpr[Boolean]) = where(condition)
  
  override def select(columns : NamedColExpr[_]*) : AggregateSelectExpr = {
    new AggregateSelectExpr(groupByColumns, columns, from, whereCond, havingCond, orderBy)
  }
  
  override def toSQL = {
    val havingSQL = if (havingCond eq null) "" else " HAVING " + havingCond.toSQL

    // if no 'group by' is specified we leave it out of the SQL, which means grouping by all columns.
    val groupBySQL = if (groupByColumns eq null) "" else " GROUP BY " + groupByColumns.map(_.nameSQL).mkString(", ")

    "SELECT " + columnsSQL + " FROM " + from.toSQL + whereSQL + groupBySQL + havingSQL + orderSQL
  }
}