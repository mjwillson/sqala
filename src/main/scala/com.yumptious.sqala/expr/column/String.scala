package com.yumptious.sqala.expr.column

// Expressions applying to String columns.
// an implicit conversion will give you StringOps on a ColExpr[String]

trait StringOps {
  protected val self : ColExpr[String]
  
  def +(other : ColExpr[String]) = new ConcatOp(self, other)
  def replace(value : ColExpr[String], replacement : ColExpr[String]) = new ReplaceOp(self, value, replacement)
  def like(value : ColExpr[String]) = new LikeOp(self, value)
}

trait StringColExpr extends ColExpr[String] with StringOps {val self = this}

class ConcatOp(a : ColExpr[String], b : ColExpr[String]) extends FunctionOp2[String,String,String]("CONCAT",a,b) with StringColExpr {}
class ReplaceOp(a : ColExpr[String], b : ColExpr[String], c : ColExpr[String]) extends FunctionOp3[String,String,String,String]("REPLACE",a,b,c) with StringColExpr {}
class LikeOp(a : ColExpr[String], b : ColExpr[String]) extends InfixBinaryOp[String,String,String]("LIKE",a,b) with StringColExpr {}