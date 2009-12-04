package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

// A named table (or view) in the database.
// Concrete subclasses should use makeColumn to define columns of the relation.
// Also recommended to assign each resulting column to a val for convenient typed access too.
abstract class Table(val name : String) extends NamedRelExpr {
  def toSQL = "`"+name+"`"

  private var columnList : List[Column[_]] = Nil
  private var columnMap  : Map[String,Column[_]] = Map()

  def columns : List[Column[_]] = columnList
  override def column[A](name : String) : Column[A] = getColumn[A](name).get
  def getColumn[A](name : String) : Option[Column[A]] = columnMap.get(name).asInstanceOf[Option[Column[A]]]
  
  protected def makeColumn[A](name : String) = {
    val col = new Column[A](this, name)
    columnList ::= col
    columnMap += (name -> col)
    col
  }
  
  override def as(name : String) = new AliasedRelExpr(this, name)
  
  def insert(pairs : ColumnAssignment[A, Literal[B]] forSome {type A; type B}*) = new Insert(this, pairs)
}