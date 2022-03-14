package io.jacfal.typesystem

object TypeMembers extends App {
  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection {
    type AnimalType // abstract type member
    type BoundedAnimal <: Animal
    type SuperBoundedAnimal >: Dog <: Animal
  }

  val ac = new AnimalCollection
  val dog: ac.AnimalType = ??? // abstract type, not working (constructor doesn't exists)
  //val cat: ac.BoundedAnimal = new Cat // not work, which animal?
  val pup: ac.SuperBoundedAnimal = new Dog // works, correctly bounded

  // alternative to generics
  trait MyList {
    type T
    def add(element: T): MyList
  }

  class NonEmptyList(value: Int) extends MyList {
    override type T = Int
    override def add(element: Int): MyList = ???
  }

  // how to restrict My List to support only some types (eg. Numbers)?
  // we can use some kind of mediator

  trait ApplicableToNumbers {
    type T <: Number // restriction for T to only numbers
  }

//  class StringList extends MyList with ApplicableToNumbers { // shouldn't compile, not number type
//    override type T = String
//    override def add(element: String): MyList = ???
//  }

  class IntList extends MyList with ApplicableToNumbers { // should compile, it's number type
    override type T = Integer
    override def add(element: Integer): MyList = ???
  }

}
