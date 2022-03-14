package io.jacfal.typesystem

object PathDependentTypes extends App {

  class Outer {
    class Inner
    object InnerObject
    type InnerType
  }

  def aMethod: Int = {
    class HelperClass

    2
  }

  // some item db example
  trait ItemLike {
    type Key
  }

  trait Item[K] extends ItemLike {
    type Key = K
  }

  trait IntItem extends Item[Int]
  trait StringItem extends Item[String]

  def get[ItemType <: ItemLike](key: ItemType#Key): ItemType = ???

  get[IntItem](26) // ok
  get[StringItem]("scala")
  //get[StringItem](25) // not work
}
