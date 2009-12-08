package com.yumptious.sqala.expr.column

// Expressions applying to String columns.
// an implicit conversion will give you StringOps on a ColExpr[String]

trait StringOps[N <: NullStatus] {
  protected val self : ColExpr[String,N]

  def +[ON <: NullStatus](other : ColExpr[String,ON]) = new ConcatOp[N,ON](self, other)
  def replace[VN <: NullStatus,RN <: NullStatus](value : ColExpr[String,VN], replacement : ColExpr[String,RN]) = new ReplaceOp[N,VN,RN](self, value, replacement)
  def like[ON <: NullStatus](value : ColExpr[String,ON]) = new LikeOp[N,ON](self, value)
}

class ConcatOp[AN <: NullStatus,BN <: NullStatus](a : ColExpr[String,AN], b : ColExpr[String,BN]) extends FunctionOp2[String,AN,String,BN,String]("CONCAT",a,b)
class ReplaceOp[AN <: NullStatus,BN <: NullStatus,CN <: NullStatus](a : ColExpr[String,AN], b : ColExpr[String,BN], c : ColExpr[String,CN]) extends FunctionOp3[String,AN,String,BN,String,CN,String]("REPLACE",a,b,c)
class LikeOp[AN <: NullStatus,BN <: NullStatus](a : ColExpr[String,AN], b : ColExpr[String,BN]) extends InfixBinaryOp[String,AN,String,BN,String]("LIKE",a,b)
