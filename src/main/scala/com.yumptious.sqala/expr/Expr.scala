package com.yumptious.sqala.expr

// Module of classes representing SQL expressions / SQL syntax nodes.
//
// These are designed to be a close fit for the syntactic structure of SQL,
// but do expose some more uniform relational interfaces on top of the SQL
// syntax nodes via traits like RelExpr
//
// The alternative approach which is not taken here, would be to represent
// pure relational algebra expressions separately, and then map them to
// SQL expressions. Although that would be interesting it's too heavy an
// abstraction for my use cases at the moment.

// Any SQL expression / syntax node
trait Expr {
  def toSQL : String
  override def toString = toSQL
}

// An SQL expression which is bound to a name.
trait NamedExpr extends Expr {
  def name : String
  
  // toSQL: the expression's definition (without mentioning the name it's bound to)
  
  // SQL to refer to the expression by its bound name only
  def nameSQL = "`"+name+"`"

  // SQL which binds the expression's definition to its name
  def bindingToNameSQL = toSQL + " AS `"+name+"`"
}

// Rather crude generator of 'fresh' names, for when an arbitrary name is needed to disambiguate
// some bit of syntax.
object FreshNames {
  private var maxSoFar = 0
  def getName = "name$"+ synchronized {
    val id = maxSoFar + 1
    maxSoFar = id
    id
  }
}