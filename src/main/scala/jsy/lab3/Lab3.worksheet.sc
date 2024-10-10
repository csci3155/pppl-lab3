/*
 * CSCI 3155: Lab 3 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab3.scala.
 */

// Imports the parse function from jsy.lab3.Parser
import jsy.lab3.Parser.{parse, parseFile}

// Imports the ast nodes
import jsy.lab3.ast._

// Imports all of the functions form jsy.student.Lab3 (your implementations in Lab3.scala)
import jsy.lab3.Lab3._

// The new language feature in Lab 3 is first-class functions.

// In JavaScript, functions can be written in a number of different
// ways. Here, we write the same identity function with different
// concrete syntax that all get parsed to the same abstract.
parse("x => x")
parse("x => { return x }")
parse("x => { return x; }")
parse("(x) => x")
parse("function (x) { return x }")

// Functions can optionally have a name, which we use for declaring
// potentially recursive functions.
parse("function id(x) { return x }")

// To simplify your implementation, we only allow single parameter
// functions. But we can get the effect of multi-parameter functions
// through currying (i.e., having a function that returns another
// function).
parse("const plus = x => y => x + y")

// Here are some more function expressions.
parse("x => { const z = 3; return x + z }")
parse("function (x) { const z = 3; return x + z }")

// Run your small-step interpreter
//iterateStep("1 + 2")
//iterateStep("x => y => x + y")
//iterateStep("const plus = x => y => x + y; plus(3)(4)")

// Parse the JavaScripty expression in your worksheet
val worksheetJsy = parseFile("src/main/scala/jsy/lab3/Lab3.worksheet.js")

// Eval the JavaScripty expression in your worksheet
//iterateStep(worksheetJsy)