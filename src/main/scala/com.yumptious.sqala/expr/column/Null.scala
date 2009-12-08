package com.yumptious.sqala.expr.column

// These are phantom (ie never-instantiated) types which are threaded through
// all column expressions in order to keep a track of whether or not that
// column may be null, at the type level.
// The sealing of these traits makes it impossible to instantiate them outside this file,
// guaranteeing that our use of implicit asInstanceOf casts between these types as type parameters, is safe

sealed trait NullStatus {
  // Type-level boolean And/Or, invoked as A#Or[B]
  // See http://svn.assembla.com/svn/metascala/src/metascala/Booleans.scala
  type Or[_ <: NullStatus] <: NullStatus
  type And[_ <: NullStatus] <: NullStatus
}

sealed trait MaybeNull extends NullStatus {
  type Or[X <: NullStatus] = MaybeNull
  type And[X <: NullStatus] = X
}

sealed trait NonNull extends NullStatus {
  type Or[X <: NullStatus] = X
  type And[X <: NullStatus] = NonNull
}

// 'IFNULL' is different to all other functions/operators in its handling of nulls; it's MaybeNull iff BOTH its arguments are MaybeNull.
// So we use NullStatus#And rather than #Or

class IfNullOp[T,AN <: NullStatus,BN <: NullStatus](a : ColExpr[T,AN], b : ColExpr[T,BN]) extends (ColExpr[T,AN],ColExpr[T,BN])(a,b) with ColExpr[T,AN#And[BN]] with FunctionOp {val name = "IFNULL"}
