package io.jacfal.advanced

import io.jacfal.advanced.FuturesPromises.User

import java.util.Date
import scala.concurrent.duration.DurationInt

object ImplicitsIntro extends App {
  val pair = "Jacob" -> "True"

  case class Person(name: String) {
    def greet = s"Hi my name is $name"
  }
  implicit def fromStringToPerson(str: String): Person = Person(str)
  println("Peter".greet)
}

object OrganizingImplicits extends App {
  // scala.Predef - implicit storage (automatically imported)
  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  println(List(1,2,3,4).sorted)

  /*
    Implicits:
     - val / var
     - object
     - accessor methods
   */

  // Exercise
  implicit val sortPersons: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0) // asc
  case class Person(name: String, age: Int)
  val persons = List(
    Person("James", 30),
    Person("Barca", 24),
    Person("George", 63),
    Person("Jimmy", 32),
    Person("Zebra", 10)
  )
  println(persons.sorted)

  /*
  Implicit scope =>
    - normal scope = LOCAL SCOPE
    - imported scope
    - companions of all types involved in the method signature
   */

  //Another exercise
  case class Purchase(nUnits: Int, unitPrice: Double)

  import UnitOrdering._
  println(List(
    Purchase(4, 2.0),
    Purchase(1, 3.1),
    Purchase(10, 1.2)
  ).sorted)

  object Purchase {
    implicit val sortTotal: Ordering[Purchase] =
      Ordering.fromLessThan((a, b) => (a.nUnits * a.unitPrice) > (b.nUnits * b.unitPrice))
  }

  object PriceOrdering {
    implicit val priceOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice > _.unitPrice)
  }

  object UnitOrdering {
    implicit val unitsOrdering: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits > _.nUnits)
  }
}

object TypeClasses extends App {
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email/></div>"
  }

  val jacob = User("Jacob", 30, "jacob@true")
  val john = User("John", 30, "jacob@true")

  trait HTMLSerializer[T] {
    def serialize(value: T): String
    def printHello(): Unit = println("hello")
  }

  implicit object UserSerializer extends HTMLSerializer[User] {
    override def serialize(value: User): String = s"<div>${value.name} (${value.age} yo) <a href=${value.email}/></div>"
  }

//  // TYPE CLASS
//  trait MyTypeClassTemplate[T] {
//    def action(value: T): String // implementation is type class instance
//  }
//
//  object MyTypeClassTemplate {
//    def apply[T](implicit instance: MyTypeClassTemplate[T]): MyTypeClassTemplate[T] = instance
//  }

//  // ** exercise **
//  trait Equal[T] { // Type class
//    def apply(a: T, b: T): Boolean
//  }
//
//  object Equal {
//    def compare[T](a: T, b: T)(implicit equal: Equal[T]): Boolean = equal(a, b)
//    def apply[T](implicit equal: Equal[T]): Equal[T] = equal // OPTIONAL access to the entire type class interface
//  }
//
//  implicit object NameEquality extends Equal[User] {
//    override def apply(a: User, b: User): Boolean = a.name == b.name // Type class instances
//  }
//
//  implicit object IntEquality extends Equal[Int] {
//    override def apply(a: Int, b: Int): Boolean = a == b
//  }
//
//  println("is equal: " + Equal[User].apply(jacob, john))
//  println("is equal: " + Equal[Int].apply(1, 1))

  // part 2
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    // access to the entire type class interface
    def apply[T](implicit serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }

  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
  }

  println(HTMLSerializer.serialize(42))

  // part 3
  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  println("Serializer test " + jacob.toHTML) // new HTMLEnrichment[User](john).toHTML(UserSerializer) -> this works because user serializer is implicit
  println(2.toHTML)
  // type class patter
}

object PimpMyLibrary extends App {
  implicit class RichInt(value: Int) {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
    def times[T](func: () => T) = {
      ???
    }
  }

  // type enrichment = pimping
  42.isEven // is compiled as new RichInt(42).isEven
  42.sqrt
  3.times(() => List(1,3))

  // pimping examples
  1 to 10
  3.seconds

  // exercises
  def someOrElse[T](predicate: => Boolean, some: T, orElse: T): T =
    if (predicate) some
    else orElse

  implicit class RichString(value: String) {
    def asInt: Seq[Int] = value.map(_.toInt)
    // move r by 2
    def encryptWithCaesar: String = {
      val upperCase = 65 to 90
      val lowerCase = 97 to 122
      val caesarShift = 2
      value.foldLeft("")((s, c) => {
        if (c == 32) s + c // space
        else {
          val caseSet = someOrElse(upperCase.contains(c), upperCase, lowerCase)
          val shift = ((c - caseSet.head) + caesarShift) % 26
          s + caseSet(shift).toChar
        }
      })
    }
  }
  println("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG".encryptWithCaesar)
}

object EqualExercise extends App {
  // ** exercise **
  trait Equal[T] { // Type class
    def apply(a: T, b: T): Boolean
  }

  object Equal {
    def compare[T](a: T, b: T)(implicit equal: Equal[T]): Boolean = equal(a, b)
    def apply[T](implicit equal: Equal[T]): Equal[T] = equal // OPTIONAL access to the entire type class interface
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name // Type class instances
  }

  implicit object IntEquality extends Equal[Int] {
    override def apply(a: Int, b: Int): Boolean = a == b
  }

  implicit object CharEquality extends Equal[Char] {
    override def apply(a: Char, b: Char): Boolean = a == b
  }

  implicit class EqualEnrichment[T](a: T) {
    def ===(b: T)(implicit comparer: Equal[T]): Boolean = comparer.apply(a, b)
    def !==(b: T)(implicit comparer: Equal[T]): Boolean = !comparer.apply(a, b)
  }

  val jacob = User("Jacob")
  val john = User("Jacob")
  println("is equal: " + Equal[User].apply(jacob, john))
  println("is equal: " + Equal[Int].apply(1, 1))
  new EqualEnrichment[User](jacob).===(john)
  println(jacob === john)
  println(jacob !== john)
  println(1 === 4)
  println('a' === 'b')

  case class Permissions(mask: String)
  implicit val defaultPermission: Permissions = Permissions("0744")

  class DoSomethingWithPermissions(p: Permissions)
  val test = new DoSomethingWithPermissions(implicitly[Permissions])
}

object JSONSerialization extends App {
  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

  // USE type class pattern
  // 1 - intermediate data types: Int, String, List, Date
  // 2 - type classes for conversion to intermediate data types
  // 3 - serialize to json

  sealed trait JsonValue { // intermediate data type
    def stringify: String
  }

  // 1 - intermediate data types: Int, String, List, Date
  final case class JsonString(value: String) extends JsonValue {
    override def stringify: String = s"${'"'}$value${'"'}"
  }
  final case class JsonNumber(value: Int) extends JsonValue {
    override def stringify: String = value.toString
  }
  final case class JsonArray(value: List[JsonValue]) extends JsonValue {
    override def stringify: String = value.map(_.stringify).mkString("[", ",", "]")
  }
  final case class JsonObject(values: Map[String, JsonValue]) extends JsonValue {
    /*
    {
      name: "John"
      age: 22
      ...
    }
     */
    override def stringify: String = values.map {
      case (key, value) => s"${'"'}$key${'"'}:${value.stringify}"
    }.mkString("{", ",", "}")
  }

  // 2 - type classes for conversion to intermediate data types
  trait JsonConverter[T] {
    def convert(value: T): JsonValue
  }

  implicit object StringConverter extends JsonConverter[String] {
    override def convert(value: String): JsonValue = JsonString(value)
  }

  implicit object NumberConverter extends JsonConverter[Int] {
    override def convert(value: Int): JsonValue = JsonNumber(value)
  }

  implicit object UserConverter extends JsonConverter[User] {
    override def convert(user: User): JsonValue = JsonObject(Map(
      "name" -> JsonString(user.name),
      "age" -> JsonNumber(user.age),
      "email" -> JsonString(user.email)
    ))
  }

  implicit object PostConverter extends JsonConverter[Post] {
      override def convert(post: Post): JsonValue = JsonObject(Map(
        "content" -> JsonString(post.content),
        "created" -> JsonString(post.createdAt.toString)
      ))
  }

  implicit object FeedConverter extends JsonConverter[Feed] {
        override def convert(feed: Feed): JsonValue = JsonObject(Map(
          "content" -> UserConverter.convert(feed.user),
          "posts" -> JsonArray(feed.posts.map(PostConverter.convert))
        ))
  }
  // 3 - serialize to json
  implicit class JsonOps[T](value: T) {
    def toJson(implicit converter: JsonConverter[T]): JsonValue = converter.convert(value)
  }

  // type class
  // call stringify on result
}

object MagnetPattern extends App {
  // method overloading

  class P2PRequest
  class P2PResponse
  class Serializer[T]
  trait Actor {
    def receive(statusCode: Int): Int
    def receive(p2pRequest: P2PRequest): Int
    def receive(p2pResponse: P2PResponse): Int
    def receive[T: Serializer](message: T): Int
  }

  trait MessageMagnet[Result] {
    def apply(): Result
  }
  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(request: => P2PRequest) extends MessageMagnet[Int] {
    def apply(): Int = {
      // logic for handling a p2p request
      println("Handling p2p request")
      26
    }
  }

  implicit class FromP2PResponse(response: P2PResponse) extends MessageMagnet[Int] {
    def apply(): Int = {
      // logic for handling a p2p request
      println("Handling p2p response")
      10
    }
  }

  receive(new P2PRequest)
  receive(new P2PResponse)
}

object MagnetPatternTest extends App {

}
