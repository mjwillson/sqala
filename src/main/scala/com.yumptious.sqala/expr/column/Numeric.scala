package com.yumptious.sqala.expr.column

// Expressions applying to numeric columns.
// an implicit conversion will give you NumericOps on a ColExpr[A] for numeric value types A

trait NumericOps[A,N <: NullStatus] {
  protected val self : ColExpr[A,N]

  def +[ON <: NullStatus](other : ColExpr[A,ON]) = new PlusOp[A,N,ON](self, other)
  def -[ON <: NullStatus](other : ColExpr[A,ON]) = new MinusOp[A,N,ON](self, other)
  def unary_-() = new UnaryMinusOp[A,N](self)
  def *[ON <: NullStatus](other : ColExpr[A,ON]) = new TimesOp[A,N,ON](self, other)
  def /[ON <: NullStatus](other : ColExpr[A,ON]) = new DivideOp[A,N,ON](self, other)
}

class PlusOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,T]("+",a,b)
class MinusOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,T]("-",a,b)
class TimesOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,T]("*",a,b)
class DivideOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,T]("/",a,b)
class UnaryMinusOp[T,AN <: NullStatus](a : ColExpr[T,AN]) extends PrefixUnaryOp[T,AN,T]("-",a)
