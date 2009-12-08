package com.yumptious.sqala.expr.column

// Some abstract superclasses for different types of SQL operator or function expressions.
// These all contain type-level logic to ensure that their result is MaybeNull iff any of the arguments are MaybeNull.
// See Null.scala for special functions which behave differently with nulls (IFNULL)

abstract class UnaryOp[A,AN <: NullStatus,B](a : ColExpr[A,AN]) extends Tuple1[ColExpr[A,AN]](a) with ColExpr[B,AN] {}

abstract class BinaryOp[A,AN <: NullStatus,B,BN <: NullStatus,C](a : ColExpr[A,AN], b : ColExpr[B,BN]) extends (ColExpr[A,AN],ColExpr[B,BN])(a,b) with ColExpr[C,AN#Or[BN]] {}
abstract class InfixBinaryOp[A,AN <: NullStatus,B,BN <: NullStatus,C](val name : String, a : ColExpr[A,AN], b : ColExpr[B,BN]) extends BinaryOp[A,AN,B,BN,C](a,b) {
  def toSQL = _1.toSQL + " " + name + " " + _2.toSQL
}

trait FunctionOp extends Product {
  val name : String
  def toSQL = {
    (0 until productArity).map(i =>
      productElement(i).asInstanceOf[ColExpr[_,_ <: NullStatus]].toSQL
    ).mkString(name+"(", ", ", ")")
  }
}

abstract class PrefixUnaryOp[A,AN <: NullStatus,B](val name : String, a : ColExpr[A,AN]) extends UnaryOp[A,AN,B](a) {
  def toSQL = name + " " + _1.toSQL
}

abstract class FunctionOp1[A,AN <: NullStatus,B](val name : String, a : ColExpr[A,AN]) extends UnaryOp[A,AN,B](a) with FunctionOp {}
abstract class FunctionOp2[A,AN <: NullStatus,B,BN <: NullStatus,C](val name : String, a : ColExpr[A,AN], b : ColExpr[B,BN]) extends BinaryOp[A,AN,B,BN,C](a,b) with FunctionOp {}
abstract class FunctionOp3[A,AN <: NullStatus,B,BN <: NullStatus,C,CN <: NullStatus,D](val name : String, a : ColExpr[A,AN], b : ColExpr[B,BN], c : ColExpr[C,CN]) extends (ColExpr[A,AN],ColExpr[B,BN],ColExpr[C,CN])(a,b,c) with ColExpr[D,AN#Or[BN#Or[CN]]] with FunctionOp {}
