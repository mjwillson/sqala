package com.yumptious.sqala.expr.column

// Expressions applying to Boolean columns.
// an implicit conversion will give you BooleanOps on a ColExpr[Boolean]

trait BooleanOps {
  protected val self : ColExpr[Boolean]

  def and(other : ColExpr[Boolean]) = new AndOp(self, other)
  def or(other : ColExpr[Boolean]) = new OrOp(self, other)
  def not() = new NotOp(self)

  def &&(other : ColExpr[Boolean]) = new AndOp(self, other)
  def ||(other : ColExpr[Boolean]) = new OrOp(self, other)
  def unary_!() = new NotOp(self)
}

trait BooleanColExpr extends ColExpr[Boolean] with BooleanOps {val self = this}

class AndOp(a : ColExpr[Boolean], b : ColExpr[Boolean]) extends InfixBinaryOp[Boolean,Boolean,Boolean]("and",a,b) with BooleanColExpr {}
class OrOp(a : ColExpr[Boolean], b : ColExpr[Boolean]) extends InfixBinaryOp[Boolean,Boolean,Boolean]("or",a,b) with BooleanColExpr {}
class NotOp(a : ColExpr[Boolean]) extends PrefixUnaryOp[Boolean,Boolean]("not", a) with BooleanColExpr {}
