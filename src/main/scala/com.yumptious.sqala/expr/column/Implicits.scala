package com.yumptious.sqala.expr.column

object Implicits {
  implicit def booleanOps[N <: NullStatus](b : ColExpr[Boolean,N]) : BooleanOps[N] = new BooleanOps[N] {val self = b}

  implicit def shortNumericOps[N <: NullStatus](b : ColExpr[Short,N]) : NumericOps[Short,N] = new NumericOps[Short,N] {val self = b}
  implicit def intNumericOps[N <: NullStatus](b : ColExpr[Int,N]) : NumericOps[Int,N] = new NumericOps[Int,N] {val self = b}
  implicit def longNumericOps[N <: NullStatus](b : ColExpr[Long,N]) : NumericOps[Long,N] = new NumericOps[Long,N] {val self = b}

  implicit def floatNumericOps[N <: NullStatus](b : ColExpr[Float,N]) : NumericOps[Float,N] = new NumericOps[Float,N] {val self = b}
  implicit def doubleNumericOps[N <: NullStatus](b : ColExpr[Double,N]) : NumericOps[Double,N] = new NumericOps[Double,N] {val self = b}

  implicit def stringOps[N <: NullStatus](b : ColExpr[String,N]) : StringOps[N] = new StringOps[N] {val self = b}

  // Implicits to make NonNull Literals (and MaybeNull too if you want, because scala won't do nonNullToMaybeNull transitively afterwards)
  // from raw values.
  implicit def stringLiteral[N <: NullStatus](a : String) : Literal[String,N] = new StringLiteral(a)
  implicit def booleanLiteral[N <: NullStatus](a : Boolean) : Literal[Boolean,N] = new BooleanLiteral(a)
  implicit def shortLiteral[N <: NullStatus](a : Short) : Literal[Short,N] = new NumericLiteral(a)
  implicit def intLiteral[N <: NullStatus](a : Int) : Literal[Int,N] = new NumericLiteral[Int,N](a)
  implicit def longLiteral[N <: NullStatus](a : Long) : Literal[Long,N] = new NumericLiteral[Long,N](a)
  implicit def floatLiteral[N <: NullStatus](a : Float) : Literal[Float,N] = new NumericLiteral[Float,N](a)
  implicit def doubleLiteral[N <: NullStatus](a : Double) : Literal[Double,N] = new NumericLiteral[Double,N](a)

  // Implicits to make MaybeNull and NonNull literals (as appropriate) from Options.

  // This lot are weaker implicits, for when the type system only knows that the type is an Option.
  // They'll give you a MaybeNull literal, even if the Option was a Some (because this isn't known at compile time).
  // If it's known at compile time that it's a "Some", the stronger implicits below should give you
  // a NonNull from the Some literal. So use a case statement if you want to get a NonNull when it's a Some.
  implicit def optionStringLiteral(a : Option[String]) : Literal[String,MaybeNull] = a match {
    case Some(x) => new StringLiteral[MaybeNull](x); case None => new NullLiteral[String]
  }
  implicit def optionBooleanLiteral(a : Option[Boolean]) : Literal[Boolean,MaybeNull] = a match {
    case Some(x) => new BooleanLiteral[MaybeNull](x); case None => new NullLiteral[Boolean]
  }
  implicit def optionShortLiteral(a : Option[Short]) : Literal[Short,MaybeNull] = a match {
    case Some(x) => new NumericLiteral[Short,MaybeNull](x); case None => new NullLiteral[Short]
  }
  implicit def optionIntLiteral(a : Option[Int]) : Literal[Int,MaybeNull] = a match {
    case Some(x) => new NumericLiteral[Int,MaybeNull](x); case None => new NullLiteral[Int]
  }
  implicit def optionLongLiteral(a : Option[Long]) : Literal[Long,MaybeNull] = a match {
    case Some(x) => new NumericLiteral[Long,MaybeNull](x); case None => new NullLiteral[Long]
  }
  implicit def optionFloatLiteral(a : Option[Float]) : Literal[Float,MaybeNull] = a match {
    case Some(x) => new NumericLiteral[Float,MaybeNull](x); case None => new NullLiteral[Float]
  }
  implicit def optionDoubleLiteral(a : Option[Double]) : Literal[Double,MaybeNull] = a match {
    case Some(x) => new NumericLiteral[Double,MaybeNull](x); case None => new NullLiteral[Double]
  }

  // Stronger implicits for Some and None.
  // Note: if using Some literals in contexts where you want this implicit inferred, you may need to give the type
  // parameter explicitly eg Some[Int](1)
  implicit def someStringLiteral(a : Some[String]) : Literal[String,NonNull] = new StringLiteral[NonNull](a.x)
  implicit def someBooleanLiteral(a : Some[Boolean]) : Literal[Boolean,NonNull] = new BooleanLiteral[NonNull](a.x)
  implicit def someShortLiteral(a : Some[Short]) : Literal[Short,NonNull] = new NumericLiteral[Short,NonNull](a.x)
  implicit def someIntLiteral(a : Some[Int]) : Literal[Int,NonNull] = new NumericLiteral[Int,NonNull](a.x)
  implicit def someLongLiteral(a : Some[Long]) : Literal[Long,NonNull] = new NumericLiteral[Long,NonNull](a.x)
  implicit def someFloatLiteral(a : Some[Float]) : Literal[Float,NonNull] = new NumericLiteral[Float,NonNull](a.x)
  implicit def someDoubleLiteral(a : Some[Double]) : Literal[Double,NonNull] = new NumericLiteral[Double,NonNull](a.x)

  // Already covered by the weaker implicits, but this can save us a case statement at runtime:
  implicit def noneLiteral[A](n : None.type) : Literal[A,MaybeNull] = new NullLiteral[A]

  // A NonNull is always acceptable where a MaybeNull is required, and we can use an implicit cast to do this since
  // these are phantom types. See ColExpr#maybeNull. This is not done with variance and "NonNull extends MaybeNull",
  // since the type-level boolean computations won't work in combination with this kind of inheritance.
  implicit def nonNullToMaybeNull[A](e : ColExpr[A,NonNull]) : ColExpr[A,MaybeNull] = e.maybeNull
}
