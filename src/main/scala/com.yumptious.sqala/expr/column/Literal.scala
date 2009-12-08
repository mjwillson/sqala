package com.yumptious.sqala.expr.column

trait Literal[A,N <: NullStatus] extends ColExpr[A,N] {}

class NullLiteral[A] extends Literal[A,MaybeNull] {
  def toSQL = "NULL"
}

class StringLiteral[N <: NullStatus](value : String) extends Literal[String,N] with StringOps[N] {
  if (value eq null) throw new NullPointerException("StringLiteral: value may not eq null. Use NullLiteral[String] instead")
  val self = this
  def toSQL = "'" + value.toString.replace("\\", "\\\\").replace("'", "\\'") + "'"
}

class NumericLiteral[A,N <: NullStatus](value : A) extends Literal[A,N] with NumericOps[A,N] {
  val self = this
  def toSQL = value.toString
}

class BooleanLiteral[N <: NullStatus](value : Boolean) extends Literal[Boolean,N] with BooleanOps[N] {
  val self = this
  def toSQL = value.toString
}
