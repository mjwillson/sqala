package com.yumptious.sqala.expr.column

// Expressions applying to numeric columns.
// an implicit conversion will give you NumericOps on a ColExpr[A] for numeric value types A

trait NumericOps[A] {
  protected val self : ColExpr[A]

  def +(other : ColExpr[A]) = new PlusOp(self, other)
  def -(other : ColExpr[A]) = new MinusOp(self, other)
  def unary_-() = new UnaryMinusOp(self)
  def *(other : ColExpr[A]) = new TimesOp(self, other)
  def /(other : ColExpr[A]) = new DivideOp(self, other)
}

trait NumericColExpr[A] extends ColExpr[A] with NumericOps[A] {val self = this}

class PlusOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,A]("+",a,b) with NumericColExpr[A] {}
class MinusOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,A]("-",a,b) with NumericColExpr[A] {}
class TimesOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,A]("*",a,b) with NumericColExpr[A] {}
class DivideOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,A]("/",a,b) with NumericColExpr[A] {}
class UnaryMinusOp[A](a : ColExpr[A]) extends PrefixUnaryOp[A,A]("-",a) with NumericColExpr[A] {}
