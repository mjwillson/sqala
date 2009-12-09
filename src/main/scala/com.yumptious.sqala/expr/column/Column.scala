package com.yumptious.sqala.expr.column

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.relation._

/* SQL expressions at the level of columns */

// Abstract trait for a column-level expression of underlying type A
trait ColExpr[A] extends Expr {
  def toSQL : String
  override def toString = toSQL

  def is(other : ColExpr[A]) = new EqualsOp(this, other)
  def ===(other : ColExpr[A]) = new EqualsOp(this, other)
  def isNot(other : ColExpr[A]) = new NotEqualsOp(this, other)
  def !==(other : ColExpr[A]) = new NotEqualsOp(this, other)
  def <(other : ColExpr[A]) = new LessThanOp(this, other)
  def <=(other : ColExpr[A]) = new LessThanEqualsOp(this, other)
  def >(other : ColExpr[A]) = new GreaterThanOp(this, other)
  def >=(other : ColExpr[A]) = new GreaterThanEqualsOp(this, other)

  def as(name : String) = new AliasColExpr[A](this, name)
  def asc : OrderExpr = new OrderExpr(this, true)
  def desc : OrderExpr = new OrderExpr(this, false)

  def ifNull(other : ColExpr[A]) = new IfNullOp(this, other)

  def isNull = new IsNullOp(this)
  def isNotNull = new IsNotNullOp(this)
  
  def in(values : Seq[ColExpr[A]]) = new InOp(this, values)
  def notIn(values : Seq[ColExpr[A]]) = new NotInOp(this, values)

  def in(relExpr : RelExpr) = new InSubqueryOp(this, relExpr.asQueryExpr)
  def notIn(relExpr : RelExpr) = new NotInSubqueryOp(this, relExpr.asQueryExpr)
}

// Trait for a column expression which is bound to a particular name
trait NamedColExpr[A] extends ColExpr[A] with NamedExpr {
  def asColumnOf(table : NamedRelExpr) : Column[A] = new Column[A](table, name)
}

// A column expression explicitly given a particular name, eg "table.column as foo" or "123 + 456 as bar"
class AliasColExpr[A](val colExpr : ColExpr[A], val name : String) extends NamedColExpr[A] {
  def toSQL = colExpr.toSQL
}

// A column which is bound to a particular NamedRelExpr by a particular name.
// Used (for example) for a column of a table, or of a named subquery
class Column[A](val table : NamedRelExpr, val name : String) extends NamedColExpr[A] {
  def toSQL = table.nameSQL + ".`" + name + "`"

  // Assignment expression of a value ColExpr to a Column. For use with Insert and Update
  def :=[V <: ColExpr[A]](value : V) = new ColumnAssignment[A, V](this, value)
  /* this would be covered by the above, it's just given so that an implicit Literal can be
     inferred for eg "Table.id := 3". The type V <: ColExpr[A] doesn't seem to
     allow the implicit to be inferred in this case in 2.7.7 */
  def :=(value : Literal[A]) = new ColumnAssignment[A, Literal[A]](this, value)
}

class EqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("=",a,b) with BooleanColExpr {}
class NotEqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("!=",a,b) with BooleanColExpr {}
class LessThanOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("<",a,b) with BooleanColExpr {}
class LessThanEqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("<=",a,b) with BooleanColExpr {}
class GreaterThanOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean](">",a,b) with BooleanColExpr {}
class GreaterThanEqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean](">=",a,b) with BooleanColExpr {}

class IfNullOp[A](a : ColExpr[A], b : ColExpr[A]) extends FunctionOp2[A,A,A]("IFNULL",a,b) {}
class IsNullOp[A](a : ColExpr[A]) extends SuffixUnaryOp[A,Boolean]("IS NULL",a) {}
class IsNotNullOp[A](a : ColExpr[A]) extends SuffixUnaryOp[A,Boolean]("IS NOT NULL",a) {}

class InOp[A](val a : ColExpr[A], val values : Seq[ColExpr[A]]) extends BooleanColExpr {
  def toSQL = a.toSQL + " IN " + values.mkString("(", ", ", ")")
}
class NotInOp[A](val a : ColExpr[A], val values : Seq[ColExpr[A]]) extends BooleanColExpr {
  def toSQL = a.toSQL + " NOT IN " + values.mkString("(", ", ", ")")
}