package com.yumptious.sqala.expr.column

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.relation._

/* SQL expressions at the level of columns */

// Abstract trait for a column-level expression of underlying type A
trait ColExpr[A, N <: NullStatus] extends Expr {
  def toSQL : String
  override def toString = toSQL

  def is[ON <: NullStatus](other : ColExpr[A,ON]) = new EqualsOp[A,N,ON](this, other)
  def ===[ON <: NullStatus](other : ColExpr[A,ON]) = new EqualsOp[A,N,ON](this, other)
  def isNot[ON <: NullStatus](other : ColExpr[A,ON]) = new NotEqualsOp[A,N,ON](this, other)
  def !==[ON <: NullStatus](other : ColExpr[A,ON]) = new NotEqualsOp[A,N,ON](this, other)
  def <[ON <: NullStatus](other : ColExpr[A,ON]) = new LessThanOp[A,N,ON](this, other)
  def <=[ON <: NullStatus](other : ColExpr[A,ON]) = new LessThanEqualsOp[A,N,ON](this, other)
  def >[ON <: NullStatus](other : ColExpr[A,ON]) = new GreaterThanOp[A,N,ON](this, other)
  def >=[ON <: NullStatus](other : ColExpr[A,ON]) = new GreaterThanEqualsOp[A,N,ON](this, other)

  def as(name : String) = new AliasColExpr[A,N](this, name)
  def asc : OrderExpr = new OrderExpr(this, true)
  def desc : OrderExpr = new OrderExpr(this, false)

  // Since the NullStatus type parameter is a phantom type, we can do these as a cast with no risk of runtime exceptions:

  // A cast which says, trust me this is non-null, even though you inferred some possibility via which it might be null.
  def nonNull = asInstanceOf[ColExpr[A,NonNull]]

  // A cast which can be used to forget the "not null" guarantee and make a NonNull expression compatible for use in MaybeNull
  // contexts. This should rarely be necessary, an an implicit in Implicits exists to make this implicit where it is required,
  // (eg assigning NonNull to a MaybeNull column)
  def maybeNull = asInstanceOf[ColExpr[A,MaybeNull]]

  def ifNull[ON <: NullStatus](other : ColExpr[A,ON]) = new IfNullOp(this, other)
}

// Trait for a column expression which is bound to a particular name
trait NamedColExpr[A,N <: NullStatus] extends ColExpr[A,N] with NamedExpr {
  def asColumnOf(table : NamedRelExpr) : Column[A,N] = new Column[A,N](table, name)
}

// A column expression explicitly given a particular name, eg "table.column as foo" or "123 + 456 as bar"
class AliasColExpr[A,N <: NullStatus](val colExpr : ColExpr[A,N], val name : String) extends NamedColExpr[A,N] {
  def toSQL = colExpr.toSQL
}

// A column which is bound to a particular NamedRelExpr by a particular name.
// Used (for example) for a column of a table, or of a named subquery
class Column[A,N <: NullStatus](val table : NamedRelExpr, val name : String) extends NamedColExpr[A,N] {
  def toSQL = table.nameSQL + ".`" + name + "`"

  // Assignment expression of a value ColExpr to a Column. For use with Insert and Update
  def :=[V <: ColExpr[A,N]](value : V) = new ColumnAssignment[A, N, V](this, value)
  /* this would be covered by the above, it's just given so that an implicit Literal can be
     inferred for eg "Table.id := 3". The type V <: ColExpr[A] doesn't seem to
     allow the implicit to be inferred in this case in 2.7.7 */
  def :=(value : Literal[A,N]) = new ColumnAssignment[A, N, Literal[A,N]](this, value)
}

class EqualsOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,Boolean]("=",a,b)
class NotEqualsOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,Boolean]("!=",a,b)
class LessThanOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,Boolean]("<",a,b)
class LessThanEqualsOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,Boolean]("<=",a,b)
class GreaterThanOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,Boolean](">",a,b)
class GreaterThanEqualsOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends InfixBinaryOp[T,AN,T,BN,Boolean](">=",a,b)
