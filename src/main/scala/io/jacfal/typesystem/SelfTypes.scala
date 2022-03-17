package io.jacfal.typesystem

object SelfTypes extends App {
  // requiring a type to be mixed in

  trait Instrumentalist {
    def play(): Unit
  }

  trait Singer { self: Instrumentalist => // whoever implements Singer, must implement Instrumentalist too!
    // rest of implementation
    def sing(): Unit
  }

  class LeadSinger extends Singer with Instrumentalist { // valid
    override def sing(): Unit = ???
    override def play(): Unit = ???
  }

  // Illegal example...
//  class Vocalize extends Singer {
//    override def sing(): Unit = ???
//  }

  // CAKE PATTERN => "dependency injection"

  // java DI example
  class Component {
    // api
  }
  class ComponentA extends Component
  class ComponentB extends Component
  class DependentComponent(val component: Component)

  // scala CAKE PATTERN example
  trait ScalaComponent {
    // api
    def action(x: Int): String
  }

  trait ScalaDependentComponent { self: ScalaComponent =>
    def dependentAction(x: Int): String = action(x) + "yeah!"
  }

  // layer 1 - small components
  trait Picture extends ScalaComponent
  trait Stats extends ScalaComponent

  // layer 2 = compose
  trait Profile extends ScalaDependentComponent with Picture
  trait Analytics extends ScalaDependentComponent with Stats
}
