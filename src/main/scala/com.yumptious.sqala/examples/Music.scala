package com.yumptious.sqala.examples

import com.yumptious.sqala.expr.relation.Table

object Release extends Table("release") {
  val id         = makeColumn[Int]("id")
  val title      = makeColumn[String]("title")
  val explicit   = makeColumn[Boolean]("explicit")
  val label_id   = makeColumn[Int]("label_id")
}

object Label extends Table("label") {
  val id        = makeColumn[Int]("id")
  val title     = makeColumn[String]("title")
}

object Recording extends Table("recording") {
  val id        = makeColumn[Int]("id")
  val title     = makeColumn[String]("title")
  val isrc      = makeColumn[String]("isrc")
}

object Track extends Table("track") {
  val release_id   = makeColumn[Int]("release_id")
  val recording_id = makeColumn[Int]("recording_id")
  val position     = makeColumn[Int]("position")
  val duration     = makeColumn[Float]("duration")
}