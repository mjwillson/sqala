package com.yumptious.sqala.expr.column

abstract class Literal[A](val value : Option[A]) extends ColExpr[A] {
  def toSQL = value match {
    case Some(x) => nonNullToSQL(x)
    case None    => "NULL"
  }
  def nonNullToSQL(x : A) : String
}

class StringLiteral(value : Option[String]) extends Literal[String](value) with StringColExpr {
  def nonNullToSQL(x : String) = "'" + x.toString.replace("\\", "\\\\").replace("'", "\\'") + "'"
}

class NumericLiteral[A](value : Option[A]) extends Literal[A](value) with NumericColExpr[A] {
  def nonNullToSQL(x : A) = x.toString
}

class BooleanLiteral(value : Option[Boolean]) extends Literal[Boolean](value) with BooleanColExpr {
  def nonNullToSQL(x : Boolean) = x.toString
}
