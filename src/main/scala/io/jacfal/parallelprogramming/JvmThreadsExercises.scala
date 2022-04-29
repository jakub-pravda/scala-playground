package io.jacfal.parallelprogramming

import io.jacfal.parallelprogramming.ThreadUnprotectedUid.getUniqueUid

import scala.collection.mutable
import scala.util.Random

object JvmThreadsExercises {
  def thread(body: => Unit): Thread = {
    val t = new Thread {
      override def run(): Unit = body
    }
    println(s"Running thread ${t.getName}")
    t.start()
    t
  }
}

object First extends App {
  def parallel[A, B](a: => A, b: => B): (A, B) = {
    @volatile var aResult: Option[A] = None
    @volatile var bResult: Option[B] = None

    val t1 = new Thread {
      override def run(): Unit = {
        println(s"Name -> ${this.getName}")
        val r = Some(a)
        aResult = r
      }
    }

    val t2 = new Thread {
      override def run(): Unit = {
        println(s"Name -> ${this.getName}")
        val r = Some(b)
        bResult = r
      }
    }
    t1.start()
    t2.start()
    t1.join()
    t2.join()

    (aResult, bResult) match {
      case (Some(a), Some(b)) => (a, b)
      case _ => throw new IllegalArgumentException("Ooops!")
    }
  }

  val result = parallel(
    {
      println("Running A block")
      Thread.sleep(5000)
      println("A block finished")
      "Hello"
    }, {
      println("Running B block")
      Thread.sleep(1500)
      println("B block finished")
      42 })
  println(result)
}

object Second extends App {

  def periodically(duration: Long)(b: => Unit): Unit = {
    val t = JvmThreadsExercises.thread  {
      while(true) {
        b
        Thread.sleep(duration)
      }
    }
  }

  def perSecond(b: => Unit): Unit = periodically(1000)(b)

  perSecond({ println("Hello, from the other side!") })
  println("Continue...")
  Thread.sleep(3000)
}

object Third extends App {
  class SyncVar[T] {
    private var v: mutable.Queue[T] = new mutable.Queue[T]()
    private val lock = AnyRef

    def get: T = this.synchronized {
      println("Getting value...")
      if (v.isEmpty) throw new IllegalArgumentException("None value")
      else {
        v.dequeue()
      }
    }
    def put(x: T): Unit = this.synchronized {
      println("Putting value...")
      Thread.sleep(500)
      v.enqueue(x)
    }

    def getWait: T = lock.synchronized {
      if (v.isEmpty) lock.wait()
      get
    }

    def putWait(x: T): Unit = lock.synchronized {
      put(x)
      lock.notify()
    }
  }

  val sc = new SyncVar[Int]
  val producerThread = JvmThreadsExercises.thread {
    (0 until 15).foreach(sc.putWait)
  }
  val consumerThread = JvmThreadsExercises.thread {
    while(true) {
      println(s"GET ${sc.getWait}")
    }
  }
  producerThread.join()
  consumerThread.join()
}

object Seventh extends App {
  // deadlock
  class Account(val name: String, initMoney: Int) {
    private var money = initMoney

    val uniqueId: Long = getUniqueUid
    def addMoney(amount: Int): Unit = {
      println(s"Adding amount $amount to account $name")
      money += amount
    }

    def getAccountStatus: Int = money
  }

  def sendAll(accounts: Set[Account], target: Account): Unit = {
    accounts.foreach { acc =>
      if (acc.uniqueId > target.uniqueId)
        acc.synchronized(target.synchronized(target.addMoney(acc.getAccountStatus)))
      else
        target.synchronized(acc.synchronized(target.addMoney(acc.getAccountStatus)))
    }
  }

  val acc1 = new Account("James", 0)
  val acc2 = new Account("Alice", 1)
  val acc3 = new Account("Marco", 2)
  val acc4 = new Account("Polo", 3)
  val testAccounts = Set(acc2, acc3, acc4)

  val t1 = JvmThreadsExercises.thread(for (_ <- 1 until 100) sendAll(testAccounts, acc1))
  val t2 = JvmThreadsExercises.thread(for (_ <- 1 until 100) sendAll(Set(acc1), acc2))
  t1.join()
  t2.join()

  println(s"FINAL ${acc1.getAccountStatus}")
}

object Eighth extends App {
  // create prioritization for tasks
  def orderByPriority(e: (() => Unit, Int)): Int = math.abs(e._2)
  val tasks = new mutable.PriorityQueue[(() => Unit, Int)]()(Ordering.by(orderByPriority))
  def asynchronous(priority: Int)(body: => Unit): Unit = this.synchronized {
    tasks.enqueue((() => body, priority))
  }

  asynchronous(2){
    println("Hello from 2")
    Thread.sleep(1000)
    println("By from 2")
  }

  asynchronous(3){
    println("Hello from 3")
    Thread.sleep(1000)
    println("By from 3")
  }

  asynchronous(1){
    println("Hello from 1")
    Thread.sleep(1000)
    println("By from 1")
  }

  val t1 = JvmThreadsExercises.thread { tasks.dequeue()._1() }
  val t2 = JvmThreadsExercises.thread { tasks.dequeue()._1() }
  val t3 = JvmThreadsExercises.thread { tasks.dequeue()._1() }

  t1.join()
  t2.join()
  t3.join()
}

object ConcurrentMa extends App {
  class ConcurrentBiMap[K, V] {
    private val map = mutable.Map.empty[K, V]
    def put(key: K, value:V): Option[(K, V)] = this.synchronized {
      map.put(key, value) match {
        case Some(v) => Some((key, v))
        case None => None
      }
    }
    def removeKey(key: K): Option[V] = this.synchronized(map.remove(key))
    def removeValue(value: V): Option[K] = this.synchronized {
      map.find(_._2 == value) match {
        case Some(kv) =>
          map.remove(kv._1) match {
            case Some(v) => Some(kv._1)
            case None => None
          }
        case None => None
      }
    }
    def getValue(key: K): Option[V] = this.synchronized(map.get(key))
    def getKey(value: V): Option[K] = this.synchronized {
      map.find(kv => kv._2 == value) match {
        case Some(result) => Some(result._1)
        case None => None
      }
    }
    def size: Int = this.synchronized(map.size)
    def iterator: Iterator[(K, V)] = this.synchronized(map.iterator)

    def replace(k1: K, v1: V, k2: K, v2: V): Unit = ???
  }

  val cc = new ConcurrentBiMap[Int, Int]

  // test
  val r = new Random()
  val t1 = JvmThreadsExercises.thread {
    for(i <- 1 until 1000000) cc.put(i, r.nextInt())
  }

  val t2 = JvmThreadsExercises.thread {
    for(i <- 1000000 until 2000000) cc.put(i, r.nextInt())
  }

  t1.join()
  t2.join()
}
