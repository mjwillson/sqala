package com.yumptious.sqala.expr.command

import com.yumptious.sqala.expr._

/* The CUD of CRUD */

/* Marker trait for an SQL statement which has a side-effect when executed */
trait Command extends Expr {}