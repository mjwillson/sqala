package com.yumptious.sqala.expr.column

import com.yumptious.sqala.expr._

// Type-safe wrapper for an assignment pair which ensures the types match.
// The second type parameter is there to keep a track of the type of value, so we can restrict it
// to Literals only in (say) the Insert command.
class ColumnAssignment[A,V <: ColExpr[A]](val column : Column[A], val value : V) extends Expr {
  def toSQL = column.toSQL + " = " + value.toSQL
}