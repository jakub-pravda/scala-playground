package io.jacfal.algorithms.graphs

import scala.annotation.tailrec

object GraphsProblems extends App {
  type Graph[T]= Map[T, Set[T]]

  def outDegree[T](graph: Graph[T], node: T): Int = graph.get(node) match {
    case None => 0
    case Some(edges) => edges.size
  }

  def inDegree[T](graph: Graph[T], node: T): Int = graph.values.count(_.contains(node))

  //
  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    @tailrec
    def traversePathRecursive(nodesToCheck: Set[T], visited: Set[T]): Boolean = {
      if (nodesToCheck.isEmpty) false
      else if (nodesToCheck.head == end) true
      else {
        val visitedUpdated =  visited + nodesToCheck.head
        val nodesToCheckUpdated = (nodesToCheck.tail ++ graph.getOrElse(nodesToCheck.head, Set.empty)) -- visited
        traversePathRecursive(nodesToCheckUpdated, visitedUpdated)
      }
    }

    graph.get(start) match {
      case None => false
      case Some(nodes) => traversePathRecursive(nodes, Set.empty)
    }
  }

  // ** TESTS ***
  val paths: Graph[String] = Map(
    "Prague" -> Set("Brno", "Pilsen"),
    "Pilsen" -> Set("Prague", "Ostrava"),
    "Brno" -> Set("Prague", "Ostrava"),
    "Ostrava" -> Set("Pilsen", "Brno"),
    "Havirov" -> Set("Ostrava")
  )

  assert(inDegree(paths, "Ostrava") == 3)
  assert(outDegree(paths, "Brno") == 2)
  println("Init graph tests OK")

  assert(isPath(paths, "Prague", "Brno"))
  assert(isPath(paths, "Brno", "Pilsen"))
  assert(!isPath(paths, "Opava", "Prague"))
  assert(isPath(paths, "Havirov", "Prague"))
  assert(!isPath(paths, "Prague", "Havirov"))
  println("Is path tests OK")
}
