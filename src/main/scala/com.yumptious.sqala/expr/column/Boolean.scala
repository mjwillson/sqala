package com.yumptious.sqala.expr.column

// Expressions applying to Boolean columns.
// an implicit conversion will give you BooleanOps on a ColExpr[Boolean]

trait BooleanOps[N <: NullStatus] {
  protected val self : ColExpr[Boolean,N]

  def and[ON <: NullStatus](other : ColExpr[Boolean,ON]) = new AndOp[N,ON](self, other)
  def or[ON <: NullStatus](other : ColExpr[Boolean,ON]) = new OrOp[N,ON](self, other)
  def not() = new NotOp[N](self)

  def &&[ON <: NullStatus](other : ColExpr[Boolean,ON]) = new AndOp[N,ON](self, other)
  def ||[ON <: NullStatus](other : ColExpr[Boolean,ON]) = new OrOp[N,ON](self, other)
  def unary_!() = new NotOp[N](self)
}

class AndOp[AN <: NullStatus,BN <: NullStatus](a : ColExpr[Boolean,AN], b : ColExpr[Boolean,BN]) extends InfixBinaryOp[Boolean,AN,Boolean,BN,Boolean]("AND",a,b) {}
class OrOp[AN <: NullStatus,BN <: NullStatus](a : ColExpr[Boolean,AN], b : ColExpr[Boolean,BN]) extends InfixBinaryOp[Boolean,AN,Boolean,BN,Boolean]("OR",a,b) {}
class NotOp[AN <: NullStatus](a : ColExpr[Boolean,AN]) extends PrefixUnaryOp[Boolean,AN,Boolean]("NOT", a) {}
