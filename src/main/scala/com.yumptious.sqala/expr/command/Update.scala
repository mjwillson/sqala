package com.yumptious.sqala.expr.command

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.relation._
import com.yumptious.sqala.expr.column._

/* Initially I had hoped to use a type like
     (Column[A], ColExpr[A]) forSome {type A}
   Rather than require a special wrapper type for column assignment pairs.
   But scala gets very weird about types like these, see
   http://lampsvn.epfl.ch/trac/scala/ticket/2765 */
class Update(val fromExpr : FromExpr, val pairs : Seq[ColumnAssignment[_, _, _]], val whereCond : ColExpr[Boolean,MaybeNull]) extends Command {
  def toSQL = {
    val pairsSQL = pairs.map(_.toSQL).mkString(", ")
    val whereSQL = if (whereCond eq null) "" else " WHERE " + whereCond.toSQL
    "UPDATE " + fromExpr.toSQL + " SET " + pairsSQL + whereSQL
  }

  def where(condition : ColExpr[Boolean,MaybeNull]) = {
    val newWhere = if (whereCond eq null) condition else new AndOp(whereCond, condition)
    new Update(fromExpr, pairs, newWhere)
  }
}
