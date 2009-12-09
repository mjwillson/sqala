package com.yumptious.sqala.expr.column

import com.yumptious.sqala.expr.relation._
import com.yumptious.sqala.expr.command._

// Column-level subquery expressions.
// Note: there's no compile-time type safety here ensuring that the QueryExpr has the
// right number of columns, or that the column has the correct type.
//
// We limit queries to one row explicitly for use in subqueries that expect one row.

class InSubqueryOp[A](val a : ColExpr[A], val query : QueryExpr) extends BooleanColExpr {
  assert(query.columns.size == 1, "InSubqueryOp: subquery must only have 1 column")

  def toSQL = a.toSQL + " IN (" + query.toSQL + ")"
}

class NotInSubqueryOp[A](val a : ColExpr[A], val query : QueryExpr) extends BooleanColExpr {
  assert(query.columns.size == 1, "NotInSubqueryOp: subquery must only have 1 column")

  def toSQL = a.toSQL + " NOT IN (" + query.toSQL + ")"
}

class ExistsSubqueryOp(val query : QueryExpr) extends BooleanColExpr {
  def toSQL = "EXISTS (" + query.limit(1).toSQL + ")"
}

class NotExistsSubqueryOp(val query : QueryExpr) extends BooleanColExpr {
  def toSQL = "NOT EXISTS (" + query.limit(1).toSQL + ")"
}

class SubqueryOp[A](val query : QueryExpr) extends ColExpr[A] {
  assert(query.columns.size == 1, "SubqueryOp: subquery must only have 1 column to be used as a column-level expression")

  def toSQL = "(" + query.limit(1).toSQL + ")"
}