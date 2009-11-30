package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._

// An expression suitable to use as the from clause of a select.
// Consists of some tree of joins (including ", " the cartesian product join) between NamedRelExpr's.
trait FromExpr extends RelExpr {
  def asFromExpr = this
}

// Wraps a single NamedRelExpr as a FromExpr
class NamedRelFromExpr(val table : NamedRelExpr) extends FromExpr {
  def toSQL = table.bindingToNameSQL
  def columns = table.columns
  def getColumn[A](name : String) = table.getColumn[A](name)
}

// FromExpr which joins two tables.
// the set of columns is the union of those of both tables.
class ProductFromExpr(val left : FromExpr, val right : FromExpr) extends FromExpr {
  def toSQL = left.toSQL + ", " + right.toSQL
  def columns = left.columns ++ right.columns
  def getColumn[A](name : String) = left.getColumn[A](name) orElse right.getColumn[A](name)
}

// A join with an 'on' clause
abstract class JoinFromExpr(left : FromExpr, right : FromExpr, val on : ColExpr[Boolean], val joinType : String) extends ProductFromExpr(left, right) {
  override def toSQL = left.toSQL + " " + joinType + " " + right.toSQL + " ON " + on.toSQL
}
class InnerJoinFromExpr(left : FromExpr, right : FromExpr, on : ColExpr[Boolean]) extends JoinFromExpr(left, right, on, "INNER JOIN") {}
class LeftOuterJoinFromExpr(left : FromExpr, right : FromExpr, on : ColExpr[Boolean]) extends JoinFromExpr(left, right, on, "LEFT OUTER JOIN") {}
class RightOuterJoinFromExpr(left : FromExpr, right : FromExpr, on : ColExpr[Boolean]) extends JoinFromExpr(left, right, on, "RIGHT OUTER JOIN") {}
class OuterJoinFromExpr(left : FromExpr, right : FromExpr, on : ColExpr[Boolean]) extends JoinFromExpr(left, right, on, "OUTER JOIN") {}