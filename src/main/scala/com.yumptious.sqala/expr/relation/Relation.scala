package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

// Abstract trait for a relational (in as much as SQL is truly relational) expression.
// More pragmatically: something which can be wrapped up and executed as an SQL select
// statement (via 'asSelect').
// Has a set of named columns, and supports various relational operations.
//
// We don't attempt full type-safety for columns of RelExpr. ColExpr[A]'s are stored
// in generic data structures as ColExpr[_], and a cast must be used when accessing
// columns of RelExpr's in a generic way if you want to do anything type-specific
// with the resulting column object.
//
// column[A](name) helps with this, but watch out since it does do a cast, and because
// of type erasure you won't get a ClassCastException upfront if you get it wrong.
trait RelExpr extends Expr {
  // 
  def columns : Seq[NamedColExpr[_]]
  def column[A](name : String) : NamedColExpr[A] = getColumn[A](name).get
  def getColumn[A](name : String) : Option[NamedColExpr[A]]
  
  def as(name : String) : NamedRelExpr = new NamedSubquery(asSelect, name)

  def asFromExpr : FromExpr
  def asSelect : SelectExpr = new SelectExpr(columns, asFromExpr)
  
  def select(columns : NamedColExpr[_]*) = new SelectExpr(columns, asFromExpr)
  def where(condition : ColExpr[Boolean]) : SelectExpr = new SelectExpr(columns, asFromExpr, condition)
  
  def join(other : RelExpr) = new ProductFromExpr(asFromExpr, other.asFromExpr)
  def innerJoin(other : RelExpr, on : ColExpr[Boolean]) = new InnerJoinFromExpr(asFromExpr, other.asFromExpr, on)
  def leftOuterJoin(other : RelExpr, on : ColExpr[Boolean]) = new LeftOuterJoinFromExpr(asFromExpr, other.asFromExpr, on)
  def rightOuterJoin(other : RelExpr, on : ColExpr[Boolean]) = new RightOuterJoinFromExpr(asFromExpr, other.asFromExpr, on)
  def outerJoin(other : RelExpr, on : ColExpr[Boolean]) = new OuterJoinFromExpr(asFromExpr, other.asFromExpr, on)

  def join(other : RelExpr, on : ColExpr[Boolean]) = innerJoin(other, on)
  def leftJoin(other : RelExpr, on : ColExpr[Boolean]) = leftOuterJoin(other, on)
  def rightJoin(other : RelExpr, on : ColExpr[Boolean]) = rightOuterJoin(other, on)
  
  def limit(theLimit : Int, offset : Int) : LimitedSelectExpr = asSelect.limit(theLimit, offset)
  def limit(theLimit : Int) : LimitedSelectExpr = limit(theLimit, 0)

  def update(pairs : ColumnAssignment[_, _]*) = new Update(asFromExpr, pairs, null)
  def delete() = new Delete(asFromExpr, null)
  def delete(tables : NamedRelExpr*) = new Delete(asFromExpr, tables, null)
}

// A relational expression which is bound to a particular name.
// These are suitable to use in the from clause of a select expression.
trait NamedRelExpr extends RelExpr with NamedExpr {
  def asFromExpr = new NamedRelFromExpr(this)
}

// A relational expression which has been explicitly bound to a particular name (eg "table as foo" or "(select 123) as subquery").
class AliasedRelExpr(val relExpr : RelExpr, val name : String) extends NamedRelExpr {
  def toSQL = relExpr.toSQL
  def columns = relExpr.columns.map(_ asColumnOf this)
  def getColumn[A](name : String) : Option[Column[A]] = relExpr.getColumn[A](name).map(_ asColumnOf this)
  // override to specialise the return type:
  override def column[A](name : String) : Column[A] = getColumn[A](name).get
}