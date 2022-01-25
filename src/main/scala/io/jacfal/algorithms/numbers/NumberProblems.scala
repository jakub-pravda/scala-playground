package io.jacfal.algorithms.numbers

import scala.annotation.tailrec

object NumberProblems extends App {
  def isPrime(i: Int): Boolean = {
    @tailrec
    def isPrimeRecursive(divisor: Int): Boolean = {
      if (divisor > Math.sqrt(i)) true
      else if (i % divisor == 0) false
      else isPrimeRecursive(divisor + 1)
    }

    if (i < 2) false
    else isPrimeRecursive(2)
  }

  def decompose(i: Int): List[Int] = {
    @tailrec
    def decomposeRecursive(remaining: Int, divisor: Int, acc: List[Int]): List[Int] = {
      if (divisor > Math.sqrt(remaining)) remaining :: acc
      else if (remaining % divisor == 0) decomposeRecursive(remaining / divisor, divisor, divisor :: acc)
      else decomposeRecursive(remaining, divisor + 1, acc)
    }

    decomposeRecursive(i, 2, List())
  }

  // ***************
  // ** RUN TESTS **
  // ***************

  // is prime tests
  assert(isPrime(2))
  assert(isPrime(5))
  assert(isPrime(113))
  assert(isPrime(997))
  assert(!isPrime(996))
  assert(!isPrime(12))
  assert(!isPrime(0))
  assert(!isPrime(1))
  println("Prime tests OK")

  // decompose tests
  assert(decompose(11) == List(11))
  assert(decompose(15) == List(5, 3))
  assert(decompose(16) == List(2, 2, 2, 2))
  assert(decompose(102) == List(17, 3, 2))
  println("Decompose tests OK")
}
