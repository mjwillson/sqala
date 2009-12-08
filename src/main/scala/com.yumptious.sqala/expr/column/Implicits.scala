package com.yumptious.sqala.expr.column

object Implicits {
  implicit def booleanOps(b : ColExpr[Boolean]) : BooleanOps = new BooleanOps {val self = b}

  implicit def shortNumericOps(b : ColExpr[Short]) : NumericOps[Short] = new NumericOps[Short] {val self = b}
  implicit def intNumericOps(b : ColExpr[Int]) : NumericOps[Int] = new NumericOps[Int] {val self = b}
  implicit def longNumericOps(b : ColExpr[Long]) : NumericOps[Long] = new NumericOps[Long] {val self = b}

  implicit def floatNumericOps(b : ColExpr[Float]) : NumericOps[Float] = new NumericOps[Float] {val self = b}
  implicit def doubleNumericOps(b : ColExpr[Double]) : NumericOps[Double] = new NumericOps[Double] {val self = b}

  implicit def stringOps(b : ColExpr[String]) : StringOps = new StringOps {val self = b}

  implicit def stringLiteral(a : String) : Literal[String] = new StringLiteral(Some(a))
  implicit def booleanLiteral(a : Boolean) : Literal[Boolean] = new BooleanLiteral(Some(a))
  implicit def shortLiteral(a : Short) : Literal[Short] = new NumericLiteral(Some(a))
  implicit def intLiteral(a : Int) : Literal[Int] = new NumericLiteral[Int](Some(a))
  implicit def longLiteral(a : Long) : Literal[Long] = new NumericLiteral[Long](Some(a))
  implicit def floatLiteral(a : Float) : Literal[Float] = new NumericLiteral[Float](Some(a))
  implicit def doubleLiteral(a : Double) : Literal[Double] = new NumericLiteral[Double](Some(a))

  implicit def optionStringLiteral(a : Option[String]) : Literal[String] = new StringLiteral(a)
  implicit def optionBooleanLiteral(a : Option[Boolean]) : Literal[Boolean] = new BooleanLiteral(a)
  implicit def optionShortLiteral(a : Option[Short]) : Literal[Short] = new NumericLiteral[Short](a)
  implicit def optionIntLiteral(a : Option[Int]) : Literal[Int] = new NumericLiteral[Int](a)
  implicit def optionLongLiteral(a : Option[Long]) : Literal[Long] = new NumericLiteral[Long](a)
  implicit def optionFloatLiteral(a : Option[Float]) : Literal[Float] = new NumericLiteral[Float](a)
  implicit def optionDoubleLiteral(a : Option[Double]) : Literal[Double] = new NumericLiteral[Double](a)
}
