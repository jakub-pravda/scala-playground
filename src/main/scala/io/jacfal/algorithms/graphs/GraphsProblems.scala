package io.jacfal.algorithms.graphs

import scala.annotation.tailrec

object GraphsProblems extends App {
  type Graph[T]= Map[T, Set[T]]

  def outDegree[T](graph: Graph[T], node: T): Int = graph.get(node) match {
    case None => 0
    case Some(edges) => edges.size
  }

  def inDegree[T](graph: Graph[T], node: T): Int = graph.values.count(_.contains(node))

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

  def findPath[T](graph: Graph[T], start: T, end: T): List[T] = {
    @tailrec
    def findPathRecursive(currentNodes: Set[T], path: List[T], remaining: Graph[T]): List[T] = {
      // TODO redesign, apply dijskstra algorithm
      val currentNodeName = path.head

      if (currentNodes.isEmpty) findPathRecursive( // STEP BACKWARD
        remaining.getOrElse(path.tail.head, Set.empty), // get nodes of previous node
        path.tail, // remove current node from path list
        remaining - currentNodeName) // remove current node from graph (explored, dead end)
      else if (currentNodes.head == end) currentNodes.head +: path
      else {
        val nextNodeName = currentNodes.head
        val nextNodes = remaining.getOrElse(nextNodeName, Set.empty)
        val remainingUpdated = remaining + (currentNodeName -> (currentNodes - nextNodeName))  // next node will be explored, remove it from set
        findPathRecursive(nextNodes, nextNodeName +: path, remainingUpdated)
      }
    }

    graph.get(start) match {
      case None => List.empty
      case Some(nodes) => findPathRecursive(nodes, List(start), graph)
    }
  }

  // ** TESTS ***
  val paths: Graph[String] = Map(
    "Prague" -> Set("CeskeBudejovice", "Brno", "Pilsen"),
    "Pilsen" -> Set("Prague", "Ostrava"),
    "Brno" -> Set("Prague", "Ostrava", "Pilsen"),
    "Ostrava" -> Set("Pilsen", "Brno"),
    "Havirov" -> Set("Ostrava"),
    "CeskeBudejovice" -> Set("Lipno"),
    "Lipno" -> Set()
  )

  assert(inDegree(paths, "Ostrava") == 3)
  assert(outDegree(paths, "Brno") == 3)
  println("Init graph tests OK")

  assert(isPath(paths, "Prague", "Brno"))
  assert(isPath(paths, "Brno", "Pilsen"))
  assert(!isPath(paths, "Opava", "Prague"))
  assert(isPath(paths, "Havirov", "Prague"))
  assert(!isPath(paths, "Prague", "Havirov"))
  println("Is path tests OK")

  println(findPath(paths, "Prague", "Brno").toString())
  println(findPath(paths, "Prague", "Pilsen").toString())
  println(findPath(paths, "Brno", "Lipno").toString())
  println(findPath(paths, "Opava", "CeskeBudejovice").toString())
  println(findPath(paths, "Pilsen", "Ostrava").toString())
}
