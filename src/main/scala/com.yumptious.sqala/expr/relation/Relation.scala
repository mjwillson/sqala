package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

// Abstract trait for a relational (in as much as SQL is truly relational) expression.
// More pragmatically: something which can be wrapped up and executed as an SQL query
// statement (via 'asQueryExpr').
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
  
  def as(name : String) : NamedRelExpr = new NamedSubquery(asQueryExpr, name)

  def asFromExpr : FromExpr
  def asQueryExpr : QueryExpr = new SelectExpr(columns, asFromExpr)
  
  def select(columns : NamedColExpr[_]*) = new SelectExpr(columns, asFromExpr)
  def where(condition : ColExpr[Boolean]) : SelectExpr = new SelectExpr(columns, asFromExpr, condition)
  
  protected def joinedTo(other : RelExpr, addFromExpr : FromExpr => FromExpr) : RelExpr = addFromExpr(asFromExpr)
  def join(other : RelExpr) : RelExpr = joinedTo(other, _ join other)
  def innerJoin(other : RelExpr, on : ColExpr[Boolean]) : RelExpr = joinedTo(other, _.innerJoin(other,on))
  def leftOuterJoin(other : RelExpr, on : ColExpr[Boolean]) : RelExpr = joinedTo(other, _.leftOuterJoin(other,on))
  def rightOuterJoin(other : RelExpr, on : ColExpr[Boolean]) : RelExpr = joinedTo(other, _.rightOuterJoin(other,on))
  def outerJoin(other : RelExpr, on : ColExpr[Boolean]) : RelExpr = joinedTo(other, _.outerJoin(other,on))

  def join(other : RelExpr, on : ColExpr[Boolean]) = innerJoin(other, on)
  def leftJoin(other : RelExpr, on : ColExpr[Boolean]) = leftOuterJoin(other, on)
  def rightJoin(other : RelExpr, on : ColExpr[Boolean]) = rightOuterJoin(other, on)
  
  def distinct : RelExpr = new DistinctSelectExpr(columns, asFromExpr, null, null)
  
  def union(other : RelExpr) = new UnionExpr(asQueryExpr, other.asQueryExpr, true)
  def unionAll(other : RelExpr) = new UnionExpr(asQueryExpr, other.asQueryExpr, false)
  def intersect(other : RelExpr) = new IntersectExpr(asQueryExpr, other.asQueryExpr, true)
  def intersectAll(other : RelExpr) = new IntersectExpr(asQueryExpr, other.asQueryExpr, false)
  def minus(other : RelExpr) = new MinusExpr(asQueryExpr, other.asQueryExpr, true)
  def minusAll(other : RelExpr) = new MinusExpr(asQueryExpr, other.asQueryExpr, false)
  
  def groupBy(groupByColumns : NamedColExpr[_]*) = new AggregateSelectExpr(groupByColumns, groupByColumns, asFromExpr, null, null, null)
  def selectGroupedBy(columns : Seq[NamedColExpr[_]], groupByColumns : Seq[NamedColExpr[_]]) : AggregateSelectExpr = {
    new AggregateSelectExpr(groupByColumns, columns, asFromExpr, null, null, null)
  }
  def selectGroupedBy(c : NamedColExpr[_], gc : NamedColExpr[_]) : AggregateSelectExpr = selectGroupedBy(Seq(c), Seq(gc))
  def selectGroupedBy(cs : Seq[NamedColExpr[_]], gc : NamedColExpr[_]) : AggregateSelectExpr = selectGroupedBy(cs, Seq(gc))
  def selectGroupedBy(c : NamedColExpr[_], gcs : Seq[NamedColExpr[_]]) : AggregateSelectExpr = selectGroupedBy(Seq(c), gcs)
  
  def selectAggregate(aggregateColumns : NamedColExpr[_]*) = selectGroupedBy(aggregateColumns, null : Seq[NamedColExpr[_]])

  // TODO: overridden versions allowing the column name to be specified
  def selectCount = selectAggregate(CountAll.as("count"))
  def selectCountDistinct(columns : ColExpr[_]*) = selectAggregate(Aggregates.countDistinct(columns : _*).as("count"))
  def selectMax[A](column : ColExpr[A]) = selectAggregate(Aggregates.max(column).as("max"))
  def selectMin[A](column : ColExpr[A]) = selectAggregate(Aggregates.min(column).as("min"))
  def selectSum[A](column : ColExpr[A]) = selectAggregate(Aggregates.sum(column).as("sum"))
  def selectAvg[A](column : ColExpr[A]) = selectAggregate(Aggregates.avg(column).as("avg"))
  def selectStdDev[A](column : ColExpr[A]) = selectAggregate(Aggregates.stdDev(column).as("stdDev"))
  
  def limit(theLimit : Int, offset : Int) : LimitedQueryExpr = new LimitedQueryExpr(asQueryExpr, theLimit, offset)
  def limit(theLimit : Int) : LimitedQueryExpr = limit(theLimit, 0)

  def update(pairs : ColumnAssignment[_, _]*) = new Update(asFromExpr, pairs, null)
  def delete() = new Delete(asFromExpr, null)
  def delete(tables : NamedRelExpr*) = new Delete(asFromExpr, tables, null)

  // subquery column expressions
  def exists = new ExistsSubqueryOp(asQueryExpr)
  def notExists = new NotExistsSubqueryOp(asQueryExpr)
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