package com.yumptious.sqala.expr.column

abstract class Literal[A](val value : A) extends ColExpr[A] {}

class StringLiteral(value : String) extends Literal[String](value) with StringColExpr {
  def toSQL = "'" + value.toString.replace("\\", "\\\\").replace("'", "\\'") + "'"
}

class NumericLiteral[A](value : A) extends Literal[A](value) with NumericColExpr[A] {
  def toSQL = value.toString
}

class BooleanLiteral(value : Boolean) extends Literal[Boolean](value) with BooleanColExpr {
  def toSQL = value.toString
}