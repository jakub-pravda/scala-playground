package io.jacfal.typesystem

object AdvancedInheritance extends App {
  // convenience
  trait Writer[T] {
    def write(value: T): Unit
  }

  trait Closable {
    def close(status: Int): Unit
  }

  trait GenericStream[T] {
    // some methods
    def foreach(f: T => Unit): Unit
  }

  def processStream[T](stream: GenericStream[T] with Writer[T] with Closable): Unit = {
    stream.foreach(println)
    stream.close(1)
  }

  // diamond problem
  trait Animal { def name: String }
  trait Lion extends Animal { override def name: String = "Lion"}
  trait Tiger extends Animal { override def name: String = "Tiger"}
  class Mutant extends Lion with Tiger
  /*
    compiler sees ->
    Mutant extends (name = Lion) with (name = Tiger)
    ! LAST override gets pick
   */

  // the super problem + type linearization
  trait Cold {
    def print(): Unit = println("Cold")
  }

  trait Green extends Cold {
    override def print(): Unit = {
      println ("Green")
      super.print()
    }
  }

  trait Blue extends Cold {
    override def print(): Unit = {
      println ("Blue")
      super.print()
    }
  }

  class Red {
    def print(): Unit = println("Red")
  }

  class White extends Red with Green with Blue {
    override def print(): Unit = {
      println("White")
      super.print()
    }
  }

  println((new White).print())
}
