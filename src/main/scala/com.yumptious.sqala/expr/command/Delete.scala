package com.yumptious.sqala.expr.command

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.relation._
import com.yumptious.sqala.expr.column._

class Delete(val fromExpr : FromExpr, val tables: Seq[NamedRelExpr], val whereCond : ColExpr[Boolean,MaybeNull]) extends Command {
  def this(fromExpr : FromExpr, whereCond : ColExpr[Boolean,MaybeNull]) = this(fromExpr, fromExpr.tables, whereCond)

  def toSQL = {
    val whereSQL = if (whereCond eq null) "" else " WHERE "+whereCond.toSQL
    val tablesSQL = tables.map(_.nameSQL).mkString(", ")
    "DELETE " + tablesSQL + " FROM " + fromExpr.toSQL + whereSQL
  }

  def where(condition : ColExpr[Boolean,MaybeNull]) = {
    val newWhere = if (whereCond eq null) condition else new AndOp(whereCond, condition)
    new Delete(fromExpr, tables, newWhere)
  }
}
