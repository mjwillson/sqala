package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

// Anything which can be executed as a query or wrapped as a subquery
trait QueryExpr extends RelExpr {
  override def as(name : String) : NamedSubquery = new NamedSubquery(this, name)
  
  override def asQueryExpr = this
  
  // a query has to be wrapped as a named subquery before it can be used in the from expression of another select:
  def asFromExpr = as(FreshNames.getName).asFromExpr
}

// An SQL select statement.
// Selects a number of NamedColExpr's from a FromExpr.
// May optionally include a ColExpr[Boolean] where clause condition,
// some OrderExpr's to order by
class SelectExpr(
    val columns   : Seq[NamedColExpr[_]],
    val from      : FromExpr,
    val whereCond : ColExpr[Boolean],
    val orderBy   : Seq[OrderExpr]
  ) extends QueryExpr {

  def this(columns : Seq[NamedColExpr[_]], from : FromExpr, whereCond : ColExpr[Boolean]) = this(columns, from, whereCond, null)
  def this(columns : Seq[NamedColExpr[_]], from : FromExpr) = this(columns, from, null, null)
  
  // We override a lot of methods on SelectExpr to avoid forming new subqueries where this isn't really needed,
  // and to ensure that we pass on our where, from and order clauses as appropriate

  // Change to the columns selected can be made without call for a subquery:
  override def select(columns : NamedColExpr[_]*) = new SelectExpr(columns, from, whereCond, orderBy)

  // New where conditions can be added to the existing where clause:
  override def where(condition : ColExpr[Boolean]) = {
    val newWhere = if (whereCond eq null) condition else new AndOp(whereCond, condition)
    new SelectExpr(columns, from, newWhere, orderBy)
  }

  override def groupBy(groupByColumns : NamedColExpr[_]*) = {
    new AggregateSelectExpr(groupByColumns, groupByColumns, from, whereCond, null, orderBy)
  }

  override def selectGroupedBy(columns : Seq[NamedColExpr[_]], groupByColumns : Seq[NamedColExpr[_]]) = {
    new AggregateSelectExpr(groupByColumns, columns, from, whereCond, null, orderBy)
  }

  override def distinct : DistinctSelectExpr = new DistinctSelectExpr(columns, from, whereCond, orderBy)
  
  override def update(pairs : ColumnAssignment[_, _]*) = new Update(from, pairs, whereCond)
  override def delete() = new Delete(from, whereCond)
  override def delete(tables : NamedRelExpr*) = new Delete(from, tables, whereCond)

  // joins on selects can be pushed inside the FromExpr:
  override def joinedTo(other : RelExpr, addFromExpr : FromExpr => FromExpr) : RelExpr = {
    new SelectExpr(columns ++ other.columns, addFromExpr(from), whereCond, orderBy)
  }
  
  // memoize column lookup based on the seq of selected columns:
  lazy private val columnMap : Map[String,NamedColExpr[_]] = Map(columns map {c => (c.name, c)} : _*)
  
  def getColumn[A](name : String) = columnMap.get(name).asInstanceOf[Option[NamedColExpr[A]]]
  
  protected def whereSQL = if (whereCond eq null) "" else " WHERE " + whereCond.toSQL
  protected def orderSQL = if (orderBy eq null) "" else " ORDER BY " + orderBy.mkString(", ")
  protected def columnsSQL = columns.map(_.bindingToNameSQL).mkString(", ")
  
  def toSQL = {
    "SELECT " + columnsSQL + " FROM " + from.toSQL + whereSQL + orderSQL
  }
}

// For use in SelectExpr:
// A column expression with an ascending or descending qualifier.
class OrderExpr(val colExpr : ColExpr[_], val asc : Boolean) extends Expr {
  def toSQL = colExpr.toSQL + (if (asc) " ASC" else " DESC")
}

// Wraps a SelectExpr as a named subquery.
// Subqueries need different bracketed syntax when bound to a name, hence overriding toSQL
class NamedSubquery(query : QueryExpr, name : String) extends AliasedRelExpr(query, name) {
  override def bindingToNameSQL = "("+query.toSQL+") AS `"+name+"`"
}

class LimitedQueryExpr(val query : QueryExpr, val limit : Int, val offset : Int) extends QueryExpr {
  def toSQL = query.toSQL + " LIMIT " + limit + (if (offset > 0) " OFFSET " + offset else "")
  def getColumn[A](n : String) = query.getColumn[A](n)
  def columns = query.columns
  
  override def limit(limit : Int, offset : Int) = {
    val newOffset = this.offset + offset
    val remaining = this.limit - offset
    val newLimit = limit min remaining max 0
    new LimitedQueryExpr(query, newLimit, newOffset)
  }
}