package com.yumptious.sqala.expr.column

/* SQL aggregate functions.
   TODO: have the type system differentiate between aggregate, non-aggregate and
   could-go-either-way (ie constant) ColExpr's, and enforce appropriate usage. */

object CountAll extends NumericColExpr[Int] {
  def toSQL = "COUNT(*)"
}

class CountDistinctOp[A](val columns : ColExpr[_]*) extends NumericColExpr[Int] {
  def toSQL = columns.map(_.toSQL).mkString("COUNT(DISTINCT ", ", ", ")")
}

class MaxOp[A](a : ColExpr[A]) extends FunctionOp1[A,A]("MAX",a) {}
class MinOp[A](a : ColExpr[A]) extends FunctionOp1[A,A]("MIN",a) {}

class SumOp[A](a : ColExpr[A]) extends FunctionOp1[A,A]("SUM",a) with NumericColExpr[A] {}
class AvgOp[A](a : ColExpr[A]) extends FunctionOp1[A,Double]("AVG",a) with NumericColExpr[Double] {}
class StdDevOp[A](a : ColExpr[A]) extends FunctionOp1[A,Double]("STDDEV",a) with NumericColExpr[Double] {}

object Aggregates {
  val countAll = CountAll
  def countDistinct(columns : ColExpr[_]*) = new CountDistinctOp(columns : _*)
  def max[A](a : ColExpr[A]) = new MaxOp(a)
  def min[A](a : ColExpr[A]) = new MinOp(a)
  def sum[A](a : ColExpr[A]) = new SumOp(a)
  def avg[A](a : ColExpr[A]) = new AvgOp(a)
  def stdDev[A](a : ColExpr[A]) = new StdDevOp(a)
}