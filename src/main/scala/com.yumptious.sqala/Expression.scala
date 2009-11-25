package com.yumptious.sqala

trait Expr {
  def toSQL : String
  override def toString = toSQL
}

trait ColExpr[A] extends Expr {
  def toSQL : String
  override def toString = toSQL
  
  def is(other : ColExpr[A]) = new EqualsOp(this, other)
  def isNot(other : ColExpr[A]) = new NotEqualsOp(this, other)
  def <(other : ColExpr[A]) = new LessThanOp(this, other)
  def <=(other : ColExpr[A]) = new LessThanEqualsOp(this, other)
  def >(other : ColExpr[A]) = new GreaterThanOp(this, other)
  def >=(other : ColExpr[A]) = new GreaterThanEqualsOp(this, other)
  
  def as(name : String) = new AliasColExpr[A](this, name)
  def asc : OrderExpr = new OrderExpr(this, true)
  def desc : OrderExpr = new OrderExpr(this, false)
}

trait NamedColExpr[A] extends ColExpr[A] {
  val name : String
}

class AliasColExpr[A](val colExpr : ColExpr[A], val name : String) extends NamedColExpr[A] {
  def toSQL = colExpr.toSQL + " as `"+name+"`"
}

class Column[A](val table : Table, val name : String) extends NamedColExpr[A] {
  def toSQL = table.toSQL + ".`" + name + "`"
}

class Literal[A](val value : A) extends ColExpr[A] {
  def toSQL = value.toString // todo
}

abstract class UnaryOp[A,B](a : ColExpr[A]) extends Tuple1[ColExpr[A]](a) with ColExpr[B] {}
abstract class BinaryOp[A,B,C](a : ColExpr[A], b : ColExpr[B]) extends (ColExpr[A],ColExpr[B])(a,b) with ColExpr[C] {}
abstract class InfixBinaryOp[A,B,C](val name : String, a : ColExpr[A], b : ColExpr[B]) extends BinaryOp[A,B,C](a,b) {
  def toSQL = _1.toSQL + " " + name + " " + _2.toSQL
}
trait FunctionOp extends Product {
  val name : String
  def toSQL = {
    (0 until productArity).map(i =>
      productElement(i).asInstanceOf[ColExpr[_]].toSQL
    ).mkString(name+"(", ", ", ")")
  }
}
abstract class PrefixUnaryOp[A,B](val name : String, a : ColExpr[A]) extends UnaryOp[A,B](a) {
  def toSQL = name + " " + _1.toSQL
}
abstract class FunctionOp1[A,B](val name : String, a : ColExpr[A]) extends UnaryOp[A,B](a) with FunctionOp {}
abstract class FunctionOp2[A,B,C](val name : String, a : ColExpr[A], b : ColExpr[B]) extends BinaryOp[A,B,C](a,b) with FunctionOp {}
abstract class FunctionOp3[A,B,C,D](val name : String, a : ColExpr[A], b : ColExpr[B], c : ColExpr[C]) extends (ColExpr[A],ColExpr[B],ColExpr[C])(a,b,c) with ColExpr[D] with FunctionOp {}

trait BooleanOps {
  protected val self : ColExpr[Boolean]

  def and(other : ColExpr[Boolean]) = new AndOp(self, other)
  def or(other : ColExpr[Boolean]) = new OrOp(self, other)
  def not() = new NotOp(self)

  def &&(other : ColExpr[Boolean]) = new AndOp(self, other)
  def ||(other : ColExpr[Boolean]) = new OrOp(self, other)
  def unary_!() = new NotOp(self)
}

trait BooleanColExpr extends ColExpr[Boolean] with BooleanOps {val self = this}

object Implicits {
  implicit def booleanOps(b : ColExpr[Boolean]) : BooleanOps = new BooleanOps {val self = b}
  implicit def integerOps(b : ColExpr[Int]) : IntegerOps = new IntegerOps {val self = b}
  implicit def stringOps(b : ColExpr[String]) : StringOps = new StringOps {val self = b}
  implicit def toLiteral[A](a : A) : Literal[A] = new Literal[A](a)
}

class AndOp(a : ColExpr[Boolean], b : ColExpr[Boolean]) extends InfixBinaryOp[Boolean,Boolean,Boolean]("and",a,b) with BooleanColExpr {}
class OrOp(a : ColExpr[Boolean], b : ColExpr[Boolean]) extends InfixBinaryOp[Boolean,Boolean,Boolean]("or",a,b) with BooleanColExpr {}
class NotOp(a : ColExpr[Boolean]) extends PrefixUnaryOp[Boolean,Boolean]("not", a) with BooleanColExpr {}

class EqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("=",a,b) with BooleanColExpr {}
class NotEqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("!=",a,b) with BooleanColExpr {}
class LessThanOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("<",a,b) with BooleanColExpr {}
class LessThanEqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean]("<=",a,b) with BooleanColExpr {}
class GreaterThanOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean](">",a,b) with BooleanColExpr {}
class GreaterThanEqualsOp[A](a : ColExpr[A], b : ColExpr[A]) extends InfixBinaryOp[A,A,Boolean](">=",a,b) with BooleanColExpr {}

trait IntegerOps {
  protected val self : ColExpr[Int]

  def +(other : ColExpr[Int]) = new PlusOp(self, other)
  def -(other : ColExpr[Int]) = new MinusOp(self, other)
  def unary_-() = new UnaryMinusOp(self)
  def *(other : ColExpr[Int]) = new TimesOp(self, other)
  def /(other : ColExpr[Int]) = new DivideOp(self, other)
}

trait IntegerColExpr extends ColExpr[Int] with IntegerOps {val self = this}

class PlusOp(a : ColExpr[Int], b : ColExpr[Int]) extends InfixBinaryOp[Int,Int,Int]("+",a,b) with IntegerColExpr {}
class MinusOp(a : ColExpr[Int], b : ColExpr[Int]) extends InfixBinaryOp[Int,Int,Int]("-",a,b) with IntegerColExpr {}
class TimesOp(a : ColExpr[Int], b : ColExpr[Int]) extends InfixBinaryOp[Int,Int,Int]("*",a,b) with IntegerColExpr {}
class DivideOp(a : ColExpr[Int], b : ColExpr[Int]) extends InfixBinaryOp[Int,Int,Int]("/",a,b) with IntegerColExpr {}
class UnaryMinusOp(a : ColExpr[Int]) extends PrefixUnaryOp[Int,Int]("-",a) with IntegerColExpr {}

trait StringOps {
  protected val self : ColExpr[String]
  
  def +(other : ColExpr[String]) = new ConcatOp(self, other)
  def replace(value : ColExpr[String], replacement : ColExpr[String]) = new ReplaceOp(self, value, replacement)
  def like(value : ColExpr[String]) = new LikeOp(self, value)
}

trait StringColExpr extends ColExpr[String] with StringOps {val self = this}

class ConcatOp(a : ColExpr[String], b : ColExpr[String]) extends FunctionOp2[String,String,String]("concat",a,b) with StringColExpr {}
class ReplaceOp(a : ColExpr[String], b : ColExpr[String], c : ColExpr[String]) extends FunctionOp3[String,String,String,String]("replace",a,b,c) with StringColExpr {}
class LikeOp(a : ColExpr[String], b : ColExpr[String]) extends InfixBinaryOp[String,String,String]("like",a,b) with StringColExpr {}

object Release extends Table("release") {
  val id        = new Column[Int](this, "id")
  val title     = new Column[String](this, "title")
  val explicit  = new Column[Boolean](this, "explicit")
  val labelId   = new Column[Int](this, "label_id")
  val columns   = List(id,title,explicit)
}
object Label extends Table("label") {
  val id        = new Column[Int](this, "id")
  val title     = new Column[String](this, "title")
  val columns   = List(id,title)
}

trait RelExpr extends Expr {
  def columns : Seq[ColExpr[_]]
  
  def map(f : RelExpr => RelExpr) = f(this)
  //def filter(f : RelExpr => ColExpr[Boolean]) = new SelectExpr(columns, f(this)
  //def flatMap[A]()
  
  // for (x <- Release; y <- Label; if x.label_id is y.id) yield (x.title, y.title)
  // Release.flatMap { case x => Label.filter(y => x.label_id is y.id).map {case y => (x.title, y.title)} }
  
  def asSelect : SelectExpr = new SelectExpr(columns, Seq(this))
  
  def join(other : RelExpr, condition : ColExpr[Boolean]) = new JoinExpr(this, other, condition)

  def where(condition : ColExpr[Boolean]) = new SelectExpr(columns, Seq(this), condition)
}

trait NamedRelExpr extends RelExpr {
  val name : String
}

abstract class Table(val name : String) extends NamedRelExpr {
  def toSQL = "`"+name+"`"

  val columns : Seq[Column[_]]
}

class JoinExpr(val left : RelExpr, val right : RelExpr, val condition : ColExpr[Boolean]) extends RelExpr {
  def columns = left.columns ++ right.columns
  def toSQL = left.toSQL + " join " + right.toSQL + " on " + condition.toSQL
}

class OrderExpr(val colExpr : ColExpr[_], val asc : Boolean) extends Expr {
  def toSQL = colExpr.toSQL + (if (asc) " asc" else " desc")
}

class SelectExpr(
    val columns : Seq[ColExpr[_]],
    val from : Seq[RelExpr],
    val whereCond : ColExpr[Boolean],
    val orderBy : Seq[OrderExpr],
    val limit : Option[Int],
    val offset : Int) extends RelExpr {

  def this(columns : Seq[ColExpr[_]], from : Seq[RelExpr], whereCond : ColExpr[Boolean], orderBy : Seq[OrderExpr], limit : Option[Int]) = this(columns, from, whereCond, orderBy, limit, 0)
  def this(columns : Seq[ColExpr[_]], from : Seq[RelExpr], whereCond : ColExpr[Boolean], orderBy : Seq[OrderExpr]) = this(columns, from, whereCond, orderBy, None, 0)
  def this(columns : Seq[ColExpr[_]], from : Seq[RelExpr], whereCond : ColExpr[Boolean]) = this(columns, from, whereCond, null, None, 0)
  def this(columns : Seq[ColExpr[_]], from : Seq[RelExpr]) = this(columns, from, null, null, None, 0)
  
  override def asSelect = this
  override def where(condition : ColExpr[Boolean]) = {
    val newWhere = if (whereCond eq null) condition else new AndOp(whereCond, condition)
    new SelectExpr(columns, from, newWhere, orderBy, limit, offset)
  }
  
  def toSQL = {
    val whereSQL = if (whereCond eq null) "" else " where " + whereCond.toSQL
    val orderSQL = if (orderBy eq null) "" else " order by " + orderBy.mkString(", ")
    val limitSQL = limit match {case Some(l) => " limit "+l; case _ => ""}
    "select " + columns.mkString(", ") + " from " + from.mkString(", ") + whereSQL + orderSQL + limitSQL
  }
}