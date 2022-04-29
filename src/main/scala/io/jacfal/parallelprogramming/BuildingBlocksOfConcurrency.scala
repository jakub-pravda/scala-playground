package io.jacfal.parallelprogramming

import java.io.File
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.concurrent.{ConcurrentHashMap, ForkJoinPool, LinkedBlockingQueue}
import scala.concurrent._
import scala.jdk.CollectionConverters.{ConcurrentMapHasAsScala, IteratorHasAsScala}

object BuildingBlocksOfConcurrency {
  def execute(body: => Unit): Unit = ExecutionContext.global.execute {
    new Runnable {
      override def run(): Unit = body
    }
  }
}

object ExecutorsCreate extends App {
  val executor = new ForkJoinPool
  executor.execute(new Runnable {
    override def run(): Unit = {
      println("Starting task...")
      Thread.sleep(1000)
      println("Ending task...")
    }
  })
  Thread.sleep(5000)
  executor.shutdown()
}

object ExecutionContextGlobal extends App {
  val ec = ExecutionContext.global
  ec.execute(new Runnable {
    override def run(): Unit = {
      println("Hello from EC...")
      Thread.sleep(1000)
      println("By from EC...")
    }
  })

  println("Running...")
  Thread.sleep(5000)
}

object ExecutionContextSleep extends App {
  for (i <- 0 until 32) BuildingBlocksOfConcurrency.execute {
    Thread.sleep(2000)
    println(s"Hello from task $i")
  }
  Thread.sleep(10000)
}

object AtomicUID extends App {
  private val uid = new AtomicLong(0L)

  def getUniqueId: Long = uid.incrementAndGet() // it uses CAS (compare and swap)

  BuildingBlocksOfConcurrency.execute {
    println(s"Asynchronously getting uid $getUniqueId")
  }
  println(s"Get unique id from main thread $getUniqueId")
}

object AtomicLock extends App {
  private val lock = new AtomicBoolean(false)
  def mySynchronized(body: => Unit): Unit = {
    while(!lock.compareAndSet(false, true)) {}
    try body finally lock.set(false)
  }
  var count = 0
  for (i <- 0 until 10) BuildingBlocksOfConcurrency.execute {
    mySynchronized(count += 1)
  }
  Thread.sleep(1000)
  println(s"Count is $count")
}

object LazyValCreate extends App {
  lazy val obj = new AnyRef
  lazy val non = s"Created by ${Thread.currentThread().getName}"

  BuildingBlocksOfConcurrency.execute {
    println(s"EC sees obj = $obj")
    println(s"EC sees non = $non")
  }
  println(s"Main sees obj = $obj")
  println(s"Main sees non = $non")
}

object ConcurrentCollectionsTest extends App {
  val messages = new LinkedBlockingQueue[String]()
  val logger = new Thread {
    setDaemon(true)

    override def run(): Unit = while (true) println(messages.take())
  }
  logger.start()
  def logMessage(s: String) = messages.offer(s)
  logMessage("Hello")
  Thread.sleep(1000)
  logMessage("world!")
  Thread.sleep(1000)
  logMessage("from other side")
}

object CollectionsIterator extends App {
  val q = new LinkedBlockingQueue[String]()
  for(i <- 1 to 5000) q.offer(i.toString)
  BuildingBlocksOfConcurrency.execute {
    val it = q.iterator()
    while (it.hasNext) println(it.next())
  }
  for (i <- 1 until 500) q.poll() // poll changing values from iterator
  // without poll - it. returns 1 .. 5000
  // with poll - it. returns tandom iterated numbers... 1,2,5,500,501...
  Thread.sleep(5000)
}
