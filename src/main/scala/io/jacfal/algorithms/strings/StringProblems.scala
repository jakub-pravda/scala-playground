package io.jacfal.algorithms.strings

import scala.annotation.tailrec

object StringProblems extends App {
  def countCharacters(s: String): Map[Char, Int] = {
    @tailrec // O(N) complexity
    def countCharactersRecursive(remaining: String, acc: Map[Char, Int]): Map[Char, Int] = {
      if (remaining.isEmpty) acc
      else {
        val update = acc.getOrElse(remaining.head, 0) + 1
        countCharactersRecursive(remaining.tail, acc + (remaining.head -> update))
      }
    }
    countCharactersRecursive(s, Map.empty)
  }

  def ransomNote(note: String, magazine: String): Boolean = {
    @tailrec
    def isInMagazineRecursive(remaining: String, currentMagazine: String): Boolean = {
      if (remaining.isEmpty) true
      else if (!currentMagazine.contains(remaining.head)) false
      else isInMagazineRecursive(remaining.tail, currentMagazine.replaceFirst(remaining.head.toString, ""))
    }
    isInMagazineRecursive(note, magazine)
  }

  def ransomNote2(note: String, magazine: String): Boolean = {
    def buildMap(s: String): Map[Char, Int] = {
      s.foldLeft(Map.empty[Char, Int]) {
        case (map, char) => map + (char -> (map.getOrElse(char, 0) + 1))
      }
    }

    val noteMap = buildMap(note)
    val magazineMap = buildMap(magazine)
    noteMap.keySet.forall(k => noteMap.getOrElse(k, 0) <= magazineMap.getOrElse(k, 0))
  }

  // ** TESTS **

  // count characters tests
  assert(countCharacters("Hello, World!").toString=="HashMap(e -> 1, ! -> 1,   -> 1, , -> 1, l -> 3, H -> 1, W -> 1, r -> 1, o -> 2, d -> 1)")
  assert(countCharacters("popocatepetl").toString()=="HashMap(e -> 2, t -> 2, a -> 1, l -> 1, p -> 3, c -> 1, o -> 2)")
  println("Count characters tests OK")

  // ransom note tests
  val note = "Hello, world!"
  val magazine1 = "Hi!, lock, less, weight, over, renault, dacia"
  val magazine2 = "Hi!, lock, less, weight, over, dacia"
  assert(ransomNote(note, magazine1))
  assert(!ransomNote(note, magazine2))
  assert(ransomNote2(note, magazine1))
  assert(!ransomNote2(note, magazine2))
  println("Ransom notes tests OK")
}

object ParenthesesProblems extends App {
  def hasValidParentheses(s: String): Boolean = {
    @tailrec
    def parenthesesFinderRecursive(remaining: String, openParentheses: Int): Boolean = {
      if (remaining.isEmpty || openParentheses < 0) openParentheses == 0
      else if (remaining.head == '(') parenthesesFinderRecursive(remaining.tail, openParentheses + 1)
      else if (remaining.head == ')') parenthesesFinderRecursive(remaining.tail, openParentheses - 1)
      else parenthesesFinderRecursive(remaining.tail, openParentheses)
    }
    parenthesesFinderRecursive(s, 0)
  }

  def generateAllValidParentheses(n: Int): List[String] = {
    @tailrec
    def genParenthesesRecursive(remaining: Int, acc: Set[String]): Set[String] = {
      if (remaining == 0) acc
      else {
        val newStrings = for {
          string <- acc
          index <- 0 until string.length
        } yield {
          val (before, after) = string.splitAt(index)
          s"$before()$after"
        }
        genParenthesesRecursive(remaining - 1, newStrings)
      }
    }
    require(n >= 0)
    if (n == 0) List.empty
    else genParenthesesRecursive(n - 1, Set("()")).toList
  }

  // ** TESTS **

  // has valid parentheses tests
  assert(hasValidParentheses("(Hello, world)"))
  assert(hasValidParentheses("(My name (is Joker))"))
  assert(!hasValidParentheses(":) Smile"))
  assert(hasValidParentheses("Without parentheses"))
  assert(hasValidParentheses("()"))
  assert(hasValidParentheses("(())"))
  assert(!hasValidParentheses(")("))
  assert(!hasValidParentheses("((((()"))
  println("Has valid parentheses tests OK")

  // generate parentheses tests
  assert(generateAllValidParentheses(1) == List("()"))
  assert(generateAllValidParentheses(2) == List("()()", "(())"))
  assert(generateAllValidParentheses(3) == List("(()())", "((()))", "()()()", "(())()", "()(())"))
  println("Generate parentheses tests OK")
}
