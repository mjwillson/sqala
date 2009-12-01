package com.yumptious.sqala.expr.column

object Implicits {
  implicit def booleanOps(b : ColExpr[Boolean]) : BooleanOps = new BooleanOps {val self = b}

  implicit def shortNumericOps(b : ColExpr[Short]) : NumericOps[Short] = new NumericOps[Short] {val self = b}
  implicit def intNumericOps(b : ColExpr[Int]) : NumericOps[Int] = new NumericOps[Int] {val self = b}
  implicit def longNumericOps(b : ColExpr[Long]) : NumericOps[Long] = new NumericOps[Long] {val self = b}

  implicit def floatNumericOps(b : ColExpr[Float]) : NumericOps[Float] = new NumericOps[Float] {val self = b}
  implicit def doubleNumericOps(b : ColExpr[Double]) : NumericOps[Double] = new NumericOps[Double] {val self = b}

  implicit def stringOps(b : ColExpr[String]) : StringOps = new StringOps {val self = b}

  implicit def stringLiteral(a : String) : StringLiteral = new StringLiteral(a)
  implicit def booleanLiteral(a : Boolean) : BooleanLiteral = new BooleanLiteral(a)
  implicit def shortLiteral(a : Short) : NumericLiteral[Short] = new NumericLiteral(a)
  implicit def intLiteral(a : Int) : NumericLiteral[Int] = new NumericLiteral[Int](a)
  implicit def longLiteral(a : Long) : NumericLiteral[Long] = new NumericLiteral[Long](a)
  implicit def floatLiteral(a : Float) : NumericLiteral[Float] = new NumericLiteral[Float](a)
  implicit def doubleLiteral(a : Double) : NumericLiteral[Double] = new NumericLiteral[Double](a)
}