package com.yumptious.sqala.expr.relation

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.column._
import com.yumptious.sqala.expr.command._

// A named table (or view) in the database.
// Concrete subclasses should use makeColumn to define columns of the relation.
// Also recommended to assign each resulting column to a val for convenient typed access too.
abstract class Table(val name : String) extends NamedRelExpr {
  def toSQL = "`"+name+"`"

  private var columnList : List[Column[_,_ <: NullStatus]] = Nil
  private var columnMap  : Map[String,Column[_,_ <: NullStatus]] = Map()

  def columns : List[Column[_,_ <: NullStatus]] = columnList
  override def column[A,N <: NullStatus](name : String) : Column[A,N] = getColumn[A,N](name).get
  def getColumn[A,N <: NullStatus](name : String) : Option[Column[A,N]] = columnMap.get(name).asInstanceOf[Option[Column[A,N]]]

  protected def makeColumn[A,N <: NullStatus](name : String) = {
    val col = new Column[A,N](this, name)
    columnList ::= col
    columnMap += (name -> col)
    col
  }

  override def as(name : String) = new AliasedRelExpr(this, name)

  def insert(pairs : ColumnAssignment[A,NA,Literal[B,NB]] forSome {type A; type NA <: NullStatus; type B; type NB <: NullStatus}*) = new Insert(this, pairs)
}
