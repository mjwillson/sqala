package com.yumptious.sqala.examples

import com.yumptious.sqala.expr.relation.Table
import com.yumptious.sqala.expr.column.{NonNull,MaybeNull}

object Release extends Table("release") {
  val id         = makeColumn[Int,NonNull]("id")
  val title      = makeColumn[String,NonNull]("title")
  val explicit   = makeColumn[Boolean,MaybeNull]("explicit")
  val label_id   = makeColumn[Int,MaybeNull]("label_id")
}

object Label extends Table("label") {
  val id        = makeColumn[Int,NonNull]("id")
  val title     = makeColumn[String,NonNull]("title")
}

object Recording extends Table("recording") {
  val id        = makeColumn[Int,NonNull]("id")
  val title     = makeColumn[String,MaybeNull]("title")
  val isrc      = makeColumn[String,MaybeNull]("isrc")
}

object Track extends Table("track") {
  val release_id   = makeColumn[Int,NonNull]("release_id")
  val recording_id = makeColumn[Int,NonNull]("recording_id")
  val position     = makeColumn[Int,NonNull]("position")
  val duration     = makeColumn[Float,MaybeNull]("duration")
}
