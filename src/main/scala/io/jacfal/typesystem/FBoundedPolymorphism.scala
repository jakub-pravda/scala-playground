package io.jacfal.typesystem


object FBoundedPolymorphism extends App {
  // solution 1. - naive
  object Solution1 {
    trait Animal {
      def bread: Vector[Animal]
    }

    class Cat extends Animal {
      override def bread: Vector[Cat] = ??? // It's valid solution
    }

    class Dog extends Animal {
      override def bread: Vector[Cat] = ??? // ... but not error prone, for this func i want Vector of dogs
    }
  }

  // solution 2. - F- bounded polymorphism
  object Solution2 {
    trait Animal[A <: Animal[A]] { // recursive types: F-Bounded Polymorphism
      def bread: Vector[Animal[A]]
    }

    class Cat extends Animal[Cat] {
      override def bread: Vector[Animal[Cat]] = ???
    }

    class Dog extends Animal[Dog] {
      override def bread: Vector[Animal[Dog]] = ???
    }

    // using a lot at entity frameworks
    trait Entity[E <: Entity[E]] // ORM

    // but, this is still possible with F-Bounded polymorphism...
    class Giraffe extends Animal[Dog] {
      override def bread: Vector[Animal[Dog]] = ??? // but, this is mistake, I wanted Animal Giraffe
    }
  }

  // solution 3. = F-bounded polymorphism with self-types
  object Solution3 {
    trait Animal[A <: Animal[A]] { self: A =>
      def bread: Vector[Animal[A]]
    }

    class Cat extends Animal[Cat] {
      override def bread: Vector[Animal[Cat]] = ???
    }

    class Dog extends Animal[Dog] {
      override def bread: Vector[Animal[Dog]] = ???
    }

    // using a lot at entity frameworks
    trait Entity[E <: Entity[E]] // ORM

    // but, this is still possible with F-Bounded polymorphism...
    class Giraffe extends Animal[Giraffe] {
      override def bread: Vector[Animal[Giraffe]] = ??? // now giraffe must be giraffe
    }
  }

}
