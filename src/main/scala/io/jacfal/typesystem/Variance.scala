package io.jacfal.typesystem

object Variance extends App {
  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Giraffe extends Animal

  // Variance -> inheritance, type substitution of generics

  class Cage[T]

  // covariance
  class CCage[+T]
  val cCage: CCage[Animal] = new CCage[Cat]

  // invariance
  class ICage[T]
  // val icage: ICage[Animal] = new ICage[Cat]// -> Invalid
  val iCage: ICage[Animal] = new ICage[Animal]

  // contravariance
  class CoCage[-T]
  val coCage: CoCage[Cat] = new CoCage[Animal]

  class InvariantCage[T](animal: T) // invariant
  class CovariantCage[+T](val animal: T) // COVARIANT POSITION
  class InvariantVariableCage[T](var animal: T)

//  trait AnotherCovariantCage[+T] {
//    def addAnimal(animal: T) // CONTRAVARIANT POSITION
//  }

  /*
    code above can't work because I can't instantiate
    val cc: AnotherCovariantCage[Animal] = new AnotherCovariantCage[Dog]
    cc.addAnimal(new Cat) ...
   */

  trait AnotherCovariantCage[-T] {
    def addAnimal(animal: T) = true // CONTRAVARIANT POSITION
  }
  val acc: AnotherCovariantCage[Cat] = new AnotherCovariantCage[Animal] {}
  acc.addAnimal(new Cat)
  class Kitty extends Cat

  class MyList[+A] {
    def add[B >: A](element: B): MyList[B] = ??? /// widening the type
  }

  val empty = new MyList[Kitty] // List[Kitty]
  val catList = empty.add(new Cat) // List[Cat]
  val animalsList = catList.add(new Dog) // List[Animals]!!!
}

object VarianceExercise extends App {
  trait Vehicle
  class Car extends Vehicle
  class BigCar extends Car
  class Bike extends Vehicle

  val cars = Seq(
    new Car, new Car
  )
  val bikes = Seq(
    new Bike, new Bike, new Bike
  )

  // create invariant, covariant, contravariant Parking[T](things List[T])
  trait Parking[+T] {
    def park[B >: T](vehicle: B): Boolean = ???
    def impound[B >: T](vehicles: Seq[B]): Boolean = ???
    def checkVehicles(conditions: String): List[T] = ???

    def flatMap[S](f: T => Parking[S]): Parking[S] = ???
  }

  class CoParking[-T](v: Seq[T]) {
    def park(vehicle: T): Boolean = ???
    def impound(vehicles: List[T]): Boolean = ???
    def checkVehicles[S <: T](conditions: String): List[S] = ???

    def flatMap[R <: T, S](f: R => CoParking[S]): CoParking[S] = ???
  }

  // invariant parking
  class IParking[T](v: Seq[T]) extends Parking[T]
  //val invarianceTest: IParking[Car] = new IParking[Bike](bikes) -> NOT VALID
  val invarianceTest: IParking[Car] = new IParking[Car](cars)
  invarianceTest.park(new Car)
  invarianceTest.impound(cars)
  invarianceTest.checkVehicles("Go")

  // covariant parking
  class CParking[+T](v: Seq[T]) extends Parking[T]
  val covariantTest: CParking[Vehicle] = new CParking[Bike](bikes)
  covariantTest.park(new Bike)
  covariantTest.impound(bikes)
  covariantTest.checkVehicles("test")

  // contravariant parking
  val contravariantTest: CoParking[BigCar] = new CoParking[Car](cars)

  /*
  Some rules -
  1. use covariance = COLLECTION OF THINGS
  2. use contravariance = GROUP OF ACTIONS
   */


}
