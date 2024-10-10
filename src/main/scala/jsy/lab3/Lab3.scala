package jsy.lab3
import scala.util.Try

object Lab3 extends jsy.util.JsyApplication  {
  import ast._
  
  /*
   * CSCI 3155: Lab 3 
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /******************************************************/
  /**************** Regular Section *********************/
  /******************************************************/

  /* Static Scoping */

  def substitute(v: Expr, x: String, e: Expr): Expr = {
    require(isValue(v), s"substitute: v ${v} to substitute is not a value")
    def subst(e: Expr): Expr = substitute(v, x, e)
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(subst(e1))
      case Unary(uop, e1) => ???
      case Binary(bop, e1, e2) => ???
      case If(e1, e2, e3) => ???
      case Call(e1, e2) => ???
      case Var(y) => ???
      case Fun(None, y, e1) => ???
      case Fun(Some(y1), y2, e1) => ???
      case ConstDecl(y, e1, e2) => ???
    }
  }
  
  /* Iteration */

  def iterateBasic[A](acc0: A)(stepi: (A, Int) => Option[A]): A = {
    def loop(acc: A, i: Int): A = ???
    loop(acc0, 0)
  }

  def iterate[Err, A](acc0: A)(stepi: (A, Int) => Option[Either[Err, A]]): Either[Err, A] = {
    def loop(acc: A, i: Int): Either[Err, A] = ???
    loop(acc0, 0)
  }

  /* Small-Step Interpreter with Static Scoping */

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case Fun(_, _, _) => true
      case _ => ??? // delete this line when done
    }
  }

  def stepBasic(e: Expr): Expr = e match {
    /* Base Cases: Do Rules */

    // DoPrint
    case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      // ****** Your cases here
      
    /* Inductive Cases: Search Rules */
    case Print(e1) => Print(stepBasic(e1))

      // ****** Your cases here

    /* Cases that should never match. Your cases above should ensure this. */
    case Var(_) => throw new AssertionError(s"Gremlins: internal error, not closed expression $e.")
    case v if isValue(v) => throw new AssertionError(s"Gremlins: internal error, step should not be called on values $v.")
  }

  def stepCheck(e: Expr): Either[DynamicTypeError, Expr] = e match {
    /* Base Cases: Do Rules */

    // DoPrint
    case Print(v1) if isValue(v1) => println(pretty(v1)); Right(Undefined)

      // ****** Your cases here

    /* Inductive Cases: Search Rules */
    case Print(e1) => stepCheck(e1) map { e1 => Print(e1) }

      // ****** Your cases here

    /* Cases that should never match. Your cases above should ensure this. */
    case Var(_) => throw new AssertionError(s"Gremlins: internal error, not closed expression $e.")
    case v if isValue(v) => throw new AssertionError(s"Gremlins: internal error, step should not be called on values $v.")
  }

  /* IMPORTANT: Choose which version of step to use for testing by choosing one of the lines. You will pick one version to submit. */
  //def step(e: Expr): Either[DynamicTypeError, Expr] = Right(stepBasic(e))
  def step(e: Expr): Either[DynamicTypeError, Expr] = stepCheck(e)

  /******************************************************/
  /************ End Regular Section *********************/
  /******************************************************/

  /******************************************************/
  /************ Accelerated Section *********************/
  /******************************************************/

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    ???
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    ???
  }

  def stepCoerce(e: Expr): Either[DynamicTypeError, Expr] = ???

  /* Choose to use this version of step for testing uncommenting this line and commenting the definition of step above. */
  //def step(e: Expr): Either[DynamicTypeError, Expr] = stepCoerce(e)

  /******************************************************/
  /************ End Accelerated Section *****************/
  /******************************************************/

  /* External Interfaces */
  
  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

  /** Interface to take a small-step from a string. This is convenient for unit testing. */
  def oneStep(s: String): Either[DynamicTypeError, Expr] = step(Parser.parse(s))

  /** Interface to run your small-step interpreter and print out the steps of evaluation if debugging. */
  def iterateStep(e: Expr): Either[DynamicTypeError, Expr] = {
    require(closed(e), s"iterateStep: ${e} not closed")
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val v = iterate(e) { (e: Expr, n: Int) =>
      if (debug) { println(s"Step $n: $e") }
      if (isValue(e)) None else Some(step(e))
    }
    if (debug) { println("Value: " + v) }
    v
  }

  /** Interface to run your small-step interpreter from a string. This is convenient for unit testing. */
  def iterateStep(s: String): Either[DynamicTypeError, Expr] = iterateStep(Parser.parse(s))

  /** Interface for main for JsyApplication */
  def processFile(file: java.io.File): Unit = {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }

    val e1 =
      handle(None: Option[Expr]) {
        Some {
          Parser.parseFile(file)
        }
      } getOrElse {
        return
      }

    handle(()) {
      println("# Stepping ...")
      def loop(e: Expr, n: Int): Either[DynamicTypeError, Expr] = {
        println("## %4d: %s".format(n, e))
        if (isValue(e)) Right(e) else step(e) flatMap { e1 => loop(e1, n + 1) }
      }
      loop(e1, 0) match {
        case Right(v1) => println(pretty(v1))
        case Left(err) => println(err)
      }
    }
  }

}