package com.yumptious.sqala.expr.column

object Implicits {
  implicit def booleanOps(b : ColExpr[Boolean]) : BooleanOps = new BooleanOps {val self = b}

  implicit def shortNumericOps(b : ColExpr[Short]) : NumericOps[Short] = new NumericOps[Short] {val self = b}
  implicit def intNumericOps(b : ColExpr[Int]) : NumericOps[Int] = new NumericOps[Int] {val self = b}
  implicit def longNumericOps(b : ColExpr[Long]) : NumericOps[Long] = new NumericOps[Long] {val self = b}

  implicit def floatNumericOps(b : ColExpr[Float]) : NumericOps[Float] = new NumericOps[Float] {val self = b}
  implicit def doubleNumericOps(b : ColExpr[Double]) : NumericOps[Double] = new NumericOps[Double] {val self = b}

  implicit def stringOps(b : ColExpr[String]) : StringOps = new StringOps {val self = b}

  implicit def toLiteral[A](a : A) : Literal[A] = new Literal[A](a)
}