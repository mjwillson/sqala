package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

abstract class SetOpExpr(val opName : String, val a : QueryExpr, val b : QueryExpr, val isDistinct : Boolean) extends QueryExpr {
  def toSQL = "(" + a.toSQL + ") " + opName + (if (isDistinct) "" else " ALL") + " (" + b.toSQL + ")"
 
  // TODO: runtime check that a.columns == b.columns
  def getColumn[A](name : String) = a.getColumn[A](name)
  def columns = a.columns
  
  // There's a shortcut to make a 'distinct' query out of a SetOp:
  def withDistinct : SetOpExpr
  override def distinct : SetOpExpr = if (isDistinct) this else withDistinct
}

class UnionExpr(a : QueryExpr, b : QueryExpr, isDistinct : Boolean) extends SetOpExpr("UNION", a, b, isDistinct) {
  def withDistinct = new UnionExpr(a, b, true)
}

class IntersectExpr(a : QueryExpr, b : QueryExpr, isDistinct : Boolean) extends SetOpExpr("INTERSECT", a, b, isDistinct) {
  def withDistinct = new IntersectExpr(a, b, true)
}

class MinusExpr(a : QueryExpr, b : QueryExpr, isDistinct : Boolean) extends SetOpExpr("MINUS", a, b, isDistinct) {
  def withDistinct = new MinusExpr(a, b, true)
}