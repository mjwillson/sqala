package com.yumptious.sqala.expr.column

// Some abstract superclasses for different types of SQL operator or function expressions

abstract class UnaryOp[A,B](a : ColExpr[A]) extends Tuple1[ColExpr[A]](a) with ColExpr[B] {}
abstract class BinaryOp[A,B,C](a : ColExpr[A], b : ColExpr[B]) extends (ColExpr[A],ColExpr[B])(a,b) with ColExpr[C] {}
abstract class InfixBinaryOp[A,B,C](val name : String, a : ColExpr[A], b : ColExpr[B]) extends BinaryOp[A,B,C](a,b) {
  def toSQL = _1.toSQL + " " + name + " " + _2.toSQL
}

trait FunctionOp extends Product {
  val name : String
  def toSQL = {
    (0 until productArity).map(i =>
      productElement(i).asInstanceOf[ColExpr[_]].toSQL
    ).mkString(name+"(", ", ", ")")
  }
}

abstract class PrefixUnaryOp[A,B](val name : String, a : ColExpr[A]) extends UnaryOp[A,B](a) {
  def toSQL = name + " " + _1.toSQL
}

abstract class SuffixUnaryOp[A,B](val name : String, a : ColExpr[A]) extends UnaryOp[A,B](a) {
  def toSQL = _1.toSQL + " " + name
}

abstract class FunctionOp1[A,B](val name : String, a : ColExpr[A]) extends UnaryOp[A,B](a) with FunctionOp {}
abstract class FunctionOp2[A,B,C](val name : String, a : ColExpr[A], b : ColExpr[B]) extends BinaryOp[A,B,C](a,b) with FunctionOp {}
abstract class FunctionOp3[A,B,C,D](val name : String, a : ColExpr[A], b : ColExpr[B], c : ColExpr[C]) extends (ColExpr[A],ColExpr[B],ColExpr[C])(a,b,c) with ColExpr[D] with FunctionOp {}
