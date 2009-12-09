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
    // we could push the condition into the WHERE clause where it only mentions groupByColumns
    val newHaving = if (havingCond eq null) condition else new AndOp(havingCond, condition)
    new AggregateSelectExpr(groupByColumns, columns, from, whereCond, newHaving, orderBy)
  }
  def having(condition : ColExpr[Boolean]) = where(condition)
  
  override def select(columns : NamedColExpr[_]*) : AggregateSelectExpr = {
    new AggregateSelectExpr(groupByColumns, columns, from, whereCond, havingCond, orderBy)
  }
  
  // Unlike plain selects, with aggregate selects it's not always safe to shove further joins inside
  // the from clause, so we revert to the joinedTo behaviour from RelExpr. (Maybe some of SelectExpr
  // should move into PlainSelectExpr?)
  override def joinedTo(other : RelExpr, addFromExpr : FromExpr => FromExpr) = addFromExpr(asFromExpr)  
  
  override def toSQL = {
    val havingSQL = if (havingCond eq null) "" else " HAVING " + havingCond.toSQL

    // if no 'group by' is specified we leave it out of the SQL, which means grouping by all columns.
    val groupBySQL = if (groupByColumns eq null) "" else " GROUP BY " + groupByColumns.map(_.nameSQL).mkString(", ")

    "SELECT " + columnsSQL + " FROM " + from.toSQL + whereSQL + groupBySQL + havingSQL + orderSQL
  }
}

// DISTINCT is effectively a special syntax for grouping on all the columns being selected
class DistinctSelectExpr(
    columns               : Seq[NamedColExpr[_]],
    from                  : FromExpr,
    whereCond             : ColExpr[Boolean],
    orderBy               : Seq[OrderExpr]
  ) extends SelectExpr(columns, from, whereCond, orderBy) {

  // Here we can push further conditions inside the where clause, since all columns are being grouped on
  override def where(condition : ColExpr[Boolean]) : DistinctSelectExpr = {
    val newWhere = if (whereCond eq null) condition else new AndOp(whereCond, condition)
    new DistinctSelectExpr(columns, from, newWhere, orderBy)
  }

  // at the point where you try to select different columns to those which we did the 'distinct' based on,
  // we have to rewrite it as an explicit AggregateSelectExpr with a GROUP BY clause
  override def select(columns : NamedColExpr[_]*) : AggregateSelectExpr = {
    new AggregateSelectExpr(this.columns, columns, from, whereCond, null, orderBy)
  }
  
  // We also revert the 'pushing joins inside the from expression' behaviour on DistinctSelectExpr's.
  // This would almost be safe but for some corner cases where there are duplicate rows in the thing
  // being joined to... gotta love bags-instead-of-sets-based SQL
  override def joinedTo(other : RelExpr, addFromExpr : FromExpr => FromExpr) = addFromExpr(asFromExpr)  

  override def toSQL = {
    "SELECT DISTINCT " + columnsSQL + " FROM " + from.toSQL + whereSQL + orderSQL
  }
}