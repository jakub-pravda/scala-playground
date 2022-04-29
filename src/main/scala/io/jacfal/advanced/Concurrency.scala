package io.jacfal.advanced

import java.util.Random
import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

object IntroConcurrency extends App {
  // jVM threads - instance of the class
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Parallel running")
  }) // need runnable interface

  aThread.start() // gives the signal to the JVM to start a JVM thread
  // create JVM thread => OS thread

  //aThread.join(100) // blocks until aThread finishes running
  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("Hello")))
  val threadBy = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))
  threadHello.start()
  threadBy.start()
  // different runs produce different results

  // executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => (1 to 10).foreach(_ => println("Counting...")))
  pool.execute(() => (1 to 5).foreach(_ => {
      println("goodbye")
      Thread.sleep(1000)
    }))

  // pool.shutdown() // shutdown all threads from the pool, but it completes previous threads
  // pool.execute(() => println("nothing...")) // should thrown exception, because all thread are shutdown
  // pool.shutdownNow() // shutdown all threads, including running one
}

object ConcurrencyProblems extends App {
  // Concurrency problems
  // 1. race condition = vice vlaken se snazi editovat jednu promenou

  def raceCondition(): Unit = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()

    println(s"x = $x")
  }

  class BankAccount(var amount: Int) { // use @volatile for thread safe operations
    override def toString: String = s"$amount"
  }

  def buy(acc: BankAccount, thing: String, price: Int): Unit = {
    acc.amount -= price // not atomic
    println(s"I've bought $thing")
    println(s"My acc is now $acc")
  }

  def buySafe(acc: BankAccount, thing: String, price: Int): Unit = {
    // option #1 - use synchronized
    acc.synchronized {
      // no to threads can evaluate this at the same time
      acc.amount -= price
      println(s"I've bought $thing")
      println(s"My acc is now $acc")
    }

    // option #2: use @volatile
  }


  for(_ <- 1 to 1000) {
    val ba = new BankAccount(5000)
    val t1 = new Thread(() => buy(ba, "PS5", 500))
    val t2 = new Thread(() => buy(ba, "XboxOneX", 1000))

    t1.start()
    t2.start()
    Thread.sleep(100)
    println()

    if (ba.amount != 3500) println("No way... " + ba)
  }
}

object ConcurrencyProblemsExercises extends App {
  /*
  1. construct 50 "inception" threads
    Thread1 -> Thread2 -> Thread3
    print("Hello from thread #num") in REVERSE order
   */

  @tailrec
  def runInceptionThread(n: Int, acc: Seq[Thread] = Seq.empty): Unit = {
    if(n <= 0) {
      acc.foreach(t => {
        t.start()
        t.join(10)
      })
    } else {
      val t = new Thread(() => println(s"Hello from thread $n"))
      runInceptionThread(n - 1, acc :+ t)
    }
  }
  //runInceptionThread(50)

  /*
    2
   */
  var x = 0
  val threads = (1 to 100).map(_ => new Thread(() => x += 1))
  threads.foreach(_.start())
  println(x)

  /*
   3 sleep fallacy
   */
  var message = ""
  val myThread = new Thread(() => {
    Thread.sleep(1000)
    message = "Scala is awesome"
  })
  message = "Scala sucks"
  myThread.start()
  Thread.sleep(2000)
  println(message)

  // sleep is very bad approach! Better solution is use to .join
}

object ThreadCommunication extends App {
  /*
   the producer - consumer problem
   producer -> [ x ] -> consumer
   */

  class SimpleContainer {
    private var x: Int = 0
    def isEmpty: Boolean = x == 0
    def set(newX: Int): Unit = {
      x = newX
    }
    def get: Int = {
      val result = x
      x = 0
      result
    }
  }

  def producerConsumer(): Unit = {
    val container = new SimpleContainer
    val consumer = new Thread(() => {
      println("[consumer] Consumer waiting")
      container.synchronized {
        container.wait()
      }
      // container must have some value
      println("[consumer] I have consumed something... " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] Producing record")
      container.synchronized {
        Thread.sleep(1000)
        container.set(26)
        println("[producer] Computed")
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }
  //producerConsumer()

  /*
  producer -> [? ? ? ?] -> consumer example
   */

  def prodConsLargeBuffer(): Unit = {
    val capacity = 3
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int](capacity)

    val consumer = new Thread(() => {
      val random = new Random()
      while(true) {
        buffer.synchronized {
          if (buffer.isEmpty) {
            println("[consumer] Buffer empty, waiting...")
            buffer.wait()
          } else {
            val x = buffer.dequeue()
            println("[consumer] Consumed new value " + x)
            buffer.notify() //notify consumer if buffer was full
          }
        }
        Thread.sleep(random.nextInt(5000))
      }
    })

    val producer = new Thread(() => {
      val random = new Random()

      while(true) {
        buffer.synchronized {
          if (buffer.size == capacity) {
            println("[producer] Buffer is full, waiting...")
            buffer.wait()
          } else {
            val greatValue = random.nextInt(1000)
            println("[producer] Producing new value " + greatValue)
            buffer.enqueue(greatValue)
            buffer.notify() // in case that buffer was empty
          }
        }
        Thread.sleep(500)
      }
    })

    producer.start()
    consumer.start()
  }
  //prodConsLargeBuffer()

  /*
  producer1, producer2,... -> [? ? ? ?] -> consumer1, consumer2,...
  */

  def multipleProducerConsumer(): Unit = {
    val capacity = 10
    val mainBuffer: mutable.Queue[Int] = new mutable.Queue[Int](capacity)

    class Consumer(name: String, buffer: mutable.Queue[Int]) extends Thread {
      override def run(): Unit = {
        val random = new Random()
        println(s"[consumer - $name] Start consuming")
        while(true) {
          buffer.synchronized {
            while (buffer.isEmpty) {
              println(s"[consumer - $name] Buffer is empty, waiting...")
              buffer.wait()
            }

            val x = buffer.dequeue()
            println(s"[consumer - $name] Consumed value $x")
            buffer.notify() // could wakeup another producer instead consumer
          }
          Thread.sleep(random.nextInt(1000))
        }
      }
    }

    class Producer(name: String, buffer: mutable.Queue[Int]) extends Thread {
      override def run(): Unit = {
        val random = new Random()
        println(s"[producer - $name] Start producing")
        while(true) {
          buffer.synchronized {
            while (buffer.size == capacity) {
              println(s"[producer - $name] Buffer is full, waiting...")
              buffer.wait()
            }

            val newValue = random.nextInt(10000)
            buffer.enqueue(newValue)
            println(s"DEBUG capacity ${buffer.size}")
            println(s"[producer - $name] Produced new value $newValue")
            buffer.notify() // could wakeup another producer instead consumer
          }
          Thread.sleep(random.nextInt(1000))
        }
      }
    }

    val producers = Seq(
      new Producer("producer-charlie", mainBuffer),
      new Producer("producer-delta", mainBuffer),
      new Producer("producer-zulu", mainBuffer)
    )

    val consumers = Seq(
      new Consumer("consumer-alpha", mainBuffer),
      new Consumer("consumer-bravo", mainBuffer),
      new Consumer("consumer-gama", mainBuffer),
      new Consumer("consumer-papa", mainBuffer),
      new Consumer("consumer-whiskey", mainBuffer)
    )
    // start
    producers.foreach(_.start())
    consumers.foreach(_.start())
  }
  multipleProducerConsumer()
}

object FuturesPromises extends App {
  def someLongComputation: Int = {
    Thread.sleep(2000)
    43
  }

  val aFuture = Future {
    println("Go go go")
    someLongComputation
  }

  // Option[Try[Int]] - Try because Future logic could failed, option because thread shouldn't be completed

  // wait for the result
  aFuture.onComplete {
    case Success(value) => println(s"Value is ready $value")
    case Failure(exception) => println(s"Failure $exception")
  }

  println("Waiting...")
  //Thread.sleep(4000)

  // social network test
  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile): Unit = {
      println(s"User $name poking another user ${anotherProfile.name}")
    }
  }

  object SocialNetwork {
    // "database"
    val names = Map(
      "alpha" -> "Jacob",
      "bravo" -> "James",
      "charlie" -> "Barca"
    )
    val friends = Map(
      "alpha" -> "charlie"
    )
    val random = new Random

    // API
    def fetchProfile(id: String): Future[Profile] = Future { // some long another thread operation
      // fetch from teh source
      Thread.sleep(random.nextInt(500))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Unit] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // Jacob poke Barca, using functional composition
  val alphaProfile = SocialNetwork.fetchProfile("alpha")
  val justName = alphaProfile.map(f => f.name) // Future[String]
  val justProfile = alphaProfile.flatMap(f => SocialNetwork.fetchProfile(f.id)) // Future[Future[Profile]]

  for {
    jacob <- SocialNetwork.fetchProfile("alpha")
    barca <- SocialNetwork.fetchProfile("charlie")
  } yield jacob.poke(barca)

  Thread.sleep(1000)

  // bank account example
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "banking app"

    def fetchUser(name: String): Future[User] = Future {
      // simulate fetching user from DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate transaction
      Thread.sleep(500)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch user from DB
      // create transaction
      // wait for the transaction to finish
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      // need to block and wait for result
      Await.result(transactionStatusFuture, 2.seconds) // implicit conversion -> pimp my library
    }
  }
  println(BankingApp.purchase("Jacob", "PS5", "Sony", 500))

  // promises
  val promise = Promise[Int]()
  val future = promise.future

  // thread 1
  future.onComplete {
    case Success(value) => println(s"[consumer] Consuming value $value")
  }

  // thread 2
  val producer = new Thread(() => {
    println("[producer] Producing numbers...")
    Thread.sleep(500)
    promise.success(26)
    println("[producer] Done...")
  })

  producer.start()
  Thread.sleep(1000)
}

object FuturePromiseExercises extends App {
  val random = new Random()

  def getFuture(id: String): Future[String] = Future {
    println(s"Starting function $id")
    Thread.sleep(random.nextInt(10000))
    println(s"Returning result of function $id")
    id
  }

  // 1. fulfill a future IMMEDIATELY with a value
  val testFuture = Future { 42 }

  // 2. inSequence(fa, fb)
  def inSequence(fa: Future[Int], fb: Future[Int]): Unit = {
    println("running in sequence")
    val result = for {
      a <- fa
      b <- fb
    } yield {
     println(s"END $a and $b")
    }
  }

  //inSequence(getFuture("alpha"), getFuture("bravo"))
  //Thread.sleep(2000)

  // return value of first future
  def getFirst[T](fa: Future[T], fb: Future[T]): Future[T] = {
    val p = Promise[T]

//    fa.onComplete {
//      case Success(v) => try {
//        p.success(v)
//      } catch {
//        case _ =>
//      }
//      case Failure(f) => p.failure(f)
//    }
//
//    fb.onComplete {
//      case Success(v) => try {
//        p.success(v)
//      } catch {
//        case _ =>
//      }
//      case Failure(f) => p.failure(f)
//    }
    fa.onComplete(p.tryComplete)
    fb.onComplete(p.tryComplete)
    p.future
  }
//  getFirst[String](getFuture("alpha"), getFuture("bravo")).onComplete {
//    case Success(value) => println("FIRST " + value)
//  }
//  Thread.sleep(20000)

  // return value of last future
  def getLast[T](fa: Future[T], fb: Future[T]): Future[T] = {
    val p = Promise[T]

    fa.onComplete { f =>
      if (fb.isCompleted) p.tryComplete(f)
    }

    fb.onComplete { f =>
      if (fa.isCompleted) p.tryComplete(f)
    }

    p.future
  }

  getLast[String](getFuture("alpha"), getFuture("bravo")).onComplete {
    case Success(value) => println("LAST " + value)
  }
  Thread.sleep(20000)

  // retry until
  def retryUntil[A](action: () => Future[A], predicate: A => Boolean): Future[A] =
    action()
      .filter(predicate)
      .recoverWith {
        case _ => retryUntil(action, predicate)
      }
}

import scala.collection.parallel.CollectionConverters._

object ParallelUtils extends App {
  // parallel collections
  val parList = List(1,2,3).par

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  val list = (1 to 500000).toList
  val serialTime = measure {
    list.map(_ + 1)
  }

  println("SERIAL " + serialTime)

  val parallelTime = measure {
    list.par.map(_ + 1)
  }

  println("PARALLEL " + parallelTime)
}
