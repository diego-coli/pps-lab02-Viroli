package u02

import u02.Lab02.Expr
import u02.Lab02.Expr.{Add, Literal, Multiply}

object Lab02 extends App :

  /////////////////////////// TASK 2

  def mult(x: Double, y: Double): Double = x * y
  println(mult(3.0, 4.0))   // return 12

  def multCurried(x: Double)(y: Double): Double = x * y
  val multiplyBy3 = multCurried(3.0)
  println(multiplyBy3(4.0))  // return 12

  def div(x: Double, y: Double): Double = x / y
  println(div(12.0, 4.0))   // return 3

  def divCurried(y: Double)(x: Double): Double = x / y
  val divideBy3 = divCurried(3.0)
  println(divideBy3(12.0))     // return 4


  /////////////////////////// TASK 3 - a

  // lambda
  val positiveLambda: Int => String = _ match
    case x if x > 0 => "positive"
    case _ => "negative"
  println(positiveLambda(10))       // return positive

  // method sintax
  def positiveFunc(x: Int): String = x match
    case x if x > 0 => "positive"
    case _ => "negative"
  println(positiveFunc(-3))   // return negative


  /////////////////////////// TASK 3 - b/c

  val empty: String => Boolean = _ == ""
  println(empty("hello"))  // return false
  println(empty(""))      // return true

  // general solution for any predicate
  def neg(pred: String => Boolean): String => Boolean =
    (s: String) => !pred(s)

  val notEmpty1 = neg(empty)
  println(notEmpty1("hello"))     // return true

  // specific solution for empty
  def negEmpty(pred: String => Boolean): String => Boolean = pred match
    case vero if empty("") => _ != ""
    case falso if !empty("") => _ == ""

  val notEmpty2 = negEmpty(empty)
  println(notEmpty2("hello"))     // return true


  /////////////////////////// TASK 4

  // val curried
  val p1: Int => Int => Int => Boolean = x => y => z => (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  // test
  val currying1 = p1(3) // x = 3
  println(currying1(4)(4)) // y = z = 4, return true
  val currying2 = p1(3) // x = 3
  println(currying2(4)(1)) // y != z, return false

  // val non-curried
  val p2:(Int, Int, Int) => Boolean = (x, y, z) => (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  // test
  println(p2(3, 4, 4)) // return true
  println(p2(5, 4, 4)) // return false

  // def curried
  def p3(x: Int)(y: Int)(z: Int): Boolean = (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  // test
  val currying3 = p3(3) // x = 3
  println(currying3(4)(4)) // y = z = 4, return true
  val currying4 = p3(3) // x = 3
  println(currying4(4)(1)) // y != z, return false

  // def non-curried
  def p4(x: Int, y: Int, z:Int): Boolean = (x, y, z) match
    case (x, y, z) if x <= y && y == z => true
    case _ => false

  // test
  println(p4(3, 4, 4)) // return true
  println(p4(3, 6, 4)) // return false


  /////////////////////////// TASK 5

  // specific for Int
  def composeInt(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))

  // test
  val res = composeInt(_ - 1, _ * 2)(5)  // g(x) = 5 * 2,   f(g(x) = (5 * 2) - 1
  println(res)    // return 9

  // generic for any type
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    x => f(g(x))


  /////////////////////////// TASK 6

  // generic for any type (3 functions), utilizing compose()
  def composeThree[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    compose(f, compose(g, h))

  // test
  println(composeThree((_: String) + "!", _.toString, (_: Int) * 2)(3))   // return 6!

  // generic for any type (3 functions), without utilizing compose()
  def composeThree2[A, B, C, D](f: C => D, g: B => C, h: A => B): A => D =
    x => f(g(h(x)))


  /////////////////////////// TASK 7

  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1.0
    case exponent if exponent > 0 => base * power(base, exponent - 1)
    case _ => 0.0

  // test
  println(power(2.0, 3))    // return 8

  def powerNeg(base: Double, exponent: Int): Double = exponent match
    case 0 => 1.0
    case exponent if exponent > 0 => base * powerNeg(base, exponent - 1)
    case negative if exponent < 0 => 1/base * powerNeg(base, exponent + 1)
    case _ => 0.0

  //test
  println(powerNeg(2.0, -3))  // return 0.125

  def powerTail(base: Double, exponent: Int): Double =
    def help(base: Double, exponent: Int, result: Double): Double =
      if (exponent == 0)
        result
      else
        help(base, exponent - 1, result * base)

    help(base, exponent, 1.0)

  // test
  println(powerTail(4.0,2))   // return 16.0


  /////////////////////////// TASK 8

  def reverseNumber(n: Int): Int =
    def help(remaining: Int, reverse: Int): Int =
      if (remaining == 0)
        reverse
      else
        val accumulator = remaining % 10 + reverse * 10
        help(remaining / 10, accumulator)

    help(n, 0)

  // test
  println(reverseNumber(12345))  // return 54321


  /////////////////////////// TASK 9

  enum Expr:
    case Literal(constant: Int)
    case Add(e1: Expr, e2: Expr)
    case Multiply(e1: Expr, e2: Expr)

  // recursive evaluate and show expressions
  object module:
    def evaluate(expr: Expr): Int = expr match
      case Literal(constant) => constant
      case Add(e1, e2) => evaluate(e1) + evaluate(e2)
      case Multiply(e1, e2) => evaluate(e1) * evaluate(e2)

    def show(expr: Expr): String = expr match
      case Literal(constant) => constant.toString
      case Add(e1, e2) => s"(${show(e1)} + ${show(e2)})"
      case Multiply(e1, e2) => s"(${show(e1)} * ${show(e2)})"

  import module.*
  val e: Expr = Multiply(Add(Literal(2), Literal(3)), Literal(6))

  // test
  println(evaluate(e))    // return 30
  println(show(e))        // return "(2 + 3) * 6"
