package com.yumptious.sqala.expr.command

import com.yumptious.sqala.expr._
import com.yumptious.sqala.expr.relation._
import com.yumptious.sqala.expr.column._

/* We only want to allow assignments to literals for an Insert.
   Why this type: ColumnAssignment[A, Literal[B]] forSome {type A; type B}
   Really we meant ColumnAssignment[A, Literal[A]] forSome {type A}
   But scala gets this wrong. Plus the type constraint already on ColumnAssignment
   will take care of ensuring that A and B match.
   ColumnAssignment[_, Literal[_]] doesn't work either.
   All seems to relate to this bug: http://lampsvn.epfl.ch/trac/scala/ticket/2765 */
class Insert(val table : Table, val pairs : Seq[ColumnAssignment[A,NA,Literal[B,NB]] forSome {type A; type NA <: NullStatus; type B; type NB <: NullStatus}]) extends Command {
  def toSQL = {
    val colsSQL = pairs.map(_.column.toSQL).mkString(" (", ", ", ")")
    val valsSQL = pairs.map(_.value.toSQL).mkString("(", ", ", ")")

    "INSERT INTO " + table.toSQL + colsSQL + " VALUES " + valsSQL
  }
}
