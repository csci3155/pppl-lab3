/*
 * CSCI 3155: Lab 3 JavaScript Worksheet
 *
 * This worksheet is a place to experiment with JavaScript expressions.
 */

// The new language feature in Lab 3 is first-class functions.

const id = function (x) { return x };

const plus = x => y => x + y;

const j = plus(3)(4);

id("Hello, ") + j;

const factorial = function f(n) {
  return n === 0
    ? 1
    : n * f(n - 1)
};

const factorial3 = factorial(3);
factorial3;