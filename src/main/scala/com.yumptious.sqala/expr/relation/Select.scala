package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

// An SQL select statement.
// Selects a number of NamedColExpr's from a FromExpr.
// May optionally include a ColExpr[Boolean] where clause condition,
// some OrderExpr's to order by
class SelectExpr(
    val columns   : Seq[NamedColExpr[_]],
    val from      : FromExpr,
    val whereCond : ColExpr[Boolean],
    val orderBy   : Seq[OrderExpr]
  ) extends RelExpr {

  def this(columns : Seq[NamedColExpr[_]], from : FromExpr, whereCond : ColExpr[Boolean]) = this(columns, from, whereCond, null)
  def this(columns : Seq[NamedColExpr[_]], from : FromExpr) = this(columns, from, null, null)
  
  // select statement has to be wrapped as a named subquery before it can be used in the from expression of another select:
  def asFromExpr = as(FreshNames.getName).asFromExpr

  override def asSelect = this
  
  override def select(columns : NamedColExpr[_]*) = new SelectExpr(columns, from, whereCond, orderBy)
  
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

  
  
  override def update(pairs : ColumnAssignment[_, _]*) = new Update(from, pairs, whereCond)
  override def delete() = new Delete(from, whereCond)
  override def delete(tables : NamedRelExpr*) = new Delete(from, tables, whereCond)
  
  override def limit(limit : Int, offset : Int) = new LimitedSelectExpr(columns, from, whereCond, orderBy, limit, offset)
  
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
class NamedSubquery(query : SelectExpr, name : String) extends AliasedRelExpr(query, name) {
  override def bindingToNameSQL = "("+query.toSQL+") AS `"+name+"`"
}

class LimitedSelectExpr(columns : Seq[NamedColExpr[_]], from : FromExpr, whereCond : ColExpr[Boolean], orderBy : Seq[OrderExpr], val limit : Int, val offset : Int) extends SelectExpr(columns, from, whereCond, orderBy) {
  // Adding extra 'where' clauses don't actually commute, semantically, with a limit clause.
  // So this ensures we're wrapped up as a subquery first.
  // If you want to slip extra conditions inside the limit, save the limit til last!
  override def where(condition : ColExpr[Boolean]) = asFromExpr.where(condition)

  override def toSQL = super.toSQL + " LIMIT "+limit+" OFFSET "+offset
}