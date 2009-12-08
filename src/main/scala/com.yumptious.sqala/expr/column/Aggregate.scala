package com.yumptious.sqala.expr.column

/* SQL aggregate functions.
   TODO: have the type system differentiate between aggregate, non-aggregate and
   could-go-either-way (ie constant) ColExpr's, and enforce appropriate usage. */

object CountAll extends ColExpr[Int,NonNull] {
  def toSQL = "COUNT(*)"
}

class CountDistinctOp[A](val columns : ColExpr[_,_ <: NullStatus]*) extends ColExpr[Int,NonNull] {
  def toSQL = columns.map(_.toSQL).mkString("COUNT(DISTINCT ", ", ", ")")
}

// These are always MaybeNull as you can get a null where the set of rows to aggregate is empty.
// You can use .notNull if you can guarantee it will never be empty.
// You would rather hope that SUM would give 0 for no rows, but this isn't the case in MySQL at least.
class MaxOp[A](a : ColExpr[A,_]) extends FunctionOp1[A,MaybeNull,A]("MAX",a.maybeNull) {}
class MinOp[A](a : ColExpr[A,_]) extends FunctionOp1[A,MaybeNull,A]("MIN",a.maybeNull) {}
class SumOp[A](a : ColExpr[A,_]) extends FunctionOp1[A,MaybeNull,A]("SUM",a.maybeNull) {}
class AvgOp[A](a : ColExpr[A,_]) extends FunctionOp1[A,MaybeNull,Double]("AVG",a.maybeNull) {}
class StdDevOp[A](a : ColExpr[A,_]) extends FunctionOp1[A,MaybeNull,Double]("STDDEV",a.maybeNull) {}

object Aggregates {
  val countAll = CountAll
  def countDistinct(columns : ColExpr[_,_ <: NullStatus]*) = new CountDistinctOp(columns : _*)
  def max[A](a : ColExpr[A,_]) = new MaxOp(a)
  def min[A](a : ColExpr[A,_]) = new MinOp(a)
  def sum[A](a : ColExpr[A,_]) = new SumOp(a)
  def avg[A](a : ColExpr[A,_]) = new AvgOp(a)
  def stdDev[A](a : ColExpr[A,_]) = new StdDevOp(a)
}
