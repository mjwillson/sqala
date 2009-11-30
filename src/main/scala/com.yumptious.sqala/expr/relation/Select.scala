package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._

// An SQL select statement.
// Selects a number of NamedColExpr's from a FromExpr with a ColExpr[Boolean] condition.
// May optionally include OrderExpr's to order by, a limit, and an offest.
class SelectExpr(
    val columns   : Seq[NamedColExpr[_]],
    val from      : FromExpr,
    val whereCond : ColExpr[Boolean],
    val orderBy   : Seq[OrderExpr],
    val limit     : Option[Int],
    val offset    : Int
  ) extends RelExpr {

  def this(columns : Seq[NamedColExpr[_]], from : FromExpr, whereCond : ColExpr[Boolean], orderBy : Seq[OrderExpr], limit : Option[Int]) = this(columns, from, whereCond, orderBy, limit, 0)
  def this(columns : Seq[NamedColExpr[_]], from : FromExpr, whereCond : ColExpr[Boolean], orderBy : Seq[OrderExpr]) = this(columns, from, whereCond, orderBy, None, 0)
  def this(columns : Seq[NamedColExpr[_]], from : FromExpr, whereCond : ColExpr[Boolean]) = this(columns, from, whereCond, null, None, 0)
  def this(columns : Seq[NamedColExpr[_]], from : FromExpr) = this(columns, from, null, null, None, 0)
  
  // select statement has to be wrapped as a named subquery before it can be used in the from expression of another select:
  def asFromExpr = as(FreshNames.getName).asFromExpr

  override def asSelect = this
  
  override def where(condition : ColExpr[Boolean]) = {
    val newWhere = if (whereCond eq null) condition else new AndOp(whereCond, condition)
    new SelectExpr(columns, from, newWhere, orderBy, limit, offset)
  }
  
  lazy private val columnMap : Map[String,NamedColExpr[_]] = Map(columns map {c => (c.name, c)} : _*)
  
  def getColumn[A](name : String) = columnMap.get(name).asInstanceOf[Option[NamedColExpr[A]]]
  
  def toSQL = {
    val whereSQL = if (whereCond eq null) "" else " WHERE " + whereCond.toSQL
    val orderSQL = if (orderBy eq null) "" else " ORDER BY " + orderBy.mkString(", ")
    val limitSQL = limit match {case Some(l) => " LIMIT "+l; case _ => ""}
    "SELECT " + columns.map(_.bindingToNameSQL).mkString(", ") + " FROM " + from.toSQL + whereSQL + orderSQL + limitSQL
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