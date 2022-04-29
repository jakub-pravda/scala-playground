package io.jacfal.parallelprogramming

import io.jacfal.parallelprogramming.ThreadUnprotectedUid.getUniqueUid

import scala.collection.mutable

object JvmThreads {
  // less verbose thread
  def thread(body : => Unit): Thread = {
    val t = new Thread {
      override def run(): Unit = body
    }
    t.start()
    t
  }
}

object ThreadMain extends App {
  val t: Thread = Thread.currentThread()
  val name = t.getName
  println(s"Thread name => $name")

  // run thread
  val helloThread = JvmThreads.thread(println("Hello, from thread!"))
  helloThread.join()
  println("Hello, from main!")
}

// Thread states -> new, runnable, terminated

object ThreadUnprotectedUid extends App {
  var uidCount = 0L
  def getUniqueUid = { // not atomic
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def getUniqueUidSynchronized = this.synchronized { // atomic
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueUids(n: Int) = {
    val uids = for (i <- 0 until n) yield getUniqueUidSynchronized
    uids.foreach(println)
  }

  val t = JvmThreads.thread(printUniqueUids(5))
  printUniqueUids(5)
  t.join()
}

object ThreadSharedStateAccessReordering extends App {
  for (i <- 0 until 10000) {
    var a = false
    var b = false
    var x = -1
    var y = -1

    val t1 = JvmThreads.thread {
      a = true
      y = if (b) 0 else 1
    }

    val t2 = JvmThreads.thread {
      b = true
      x = if (a) 0 else 1
    }
    t1.join()
    t2.join()
    assert(!(x==1 && y==1), s"x = $x, y = $y")
  }
}

object MonitorExample extends App {
  class Account(val name: String, var money: Int) {
    val uniqueId: Long = getUniqueUid
    def add(account: Account, amount: Int): Unit = account.synchronized {
      account.money += amount
      if (amount > 10) logTransfers(account.name, amount)
    }
  }

  def send(a: Account, b: Account, n: Int, threadName: String) = a.synchronized { // DEADLOCK code example
    println(s"A synchronized from $threadName")
    b.synchronized {
      println(s"B synchronized from $threadName")
      a.money -= n
      b.money += n
    }
  }

  def sendWithoutDeadLock(a: Account, b: Account, n: Int): Unit = {
    def adjust(): Unit = {
      a.money -= n
      b.money += n
    }
    if (a.uniqueId > b.uniqueId)
      a.synchronized(b.synchronized(adjust()))
    else
      b.synchronized(a.synchronized(adjust()))
  }

  import scala.collection._
  private val transfers = mutable.ArrayBuffer[String]()
  def logTransfers(name: String, a: Int) = this.synchronized {
    transfers += s"transfer to account $name, amount = $a"
  }

  val jacob = new Account("Jacob", 0)
  val jane = new Account("Jane", 0)

  val t1 = JvmThreads.thread { jacob.add(jane, 100) }
  val t2 = JvmThreads.thread { jane.add(jacob, 300) }
  val t3 = JvmThreads.thread { jacob.add(jane, 4) }

  t1.join()
  t2.join()
  t3.join()

  transfers.foreach(println)

  // deadlock example
  println("Deadlock example")
  val a = new Account("Jack", 1000)
  val b = new Account("Jane", 2000)
  val t4 = JvmThreads.thread { for(i <- 1 until 100) send(a, b, 1, "thread 1")}
  val t5 = JvmThreads.thread { for(i <- 1 until 100) send(b, a, 1, "thread 2")}
  t4.join()
  t5.join()
  println(s"money status a = ${a.money}, b = ${b.money}")
}

object SynchronizedBadPool extends App { // bad example of synchronized pool
  private val tasks = mutable.Queue[() => Unit]()
  val worker: Thread = new Thread {
    def pool(): Option[() => Unit] = tasks.synchronized {
      if (tasks.nonEmpty)
        Some(tasks.dequeue())
      else
        None
    }
    override def run(): Unit = {
      while (true) pool() match { // better solution is to use wait (guarded blocks)
          case Some(task) => task()
          case None =>
        }
    }
  }
  worker.setName("Worker")
  worker.setDaemon(true)
  worker.start()

  def asynchronous(body: => Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
  }

  asynchronous(println("Hello!"))
  asynchronous(println("World!"))
  Thread.sleep(5000)
}

object SynchronizedGuardedBlocks extends App {
  val lock = new AnyRef
  var message: Option[String] = None

  val greeter = JvmThreads.thread {
    lock.synchronized {
      println("RUN!!!")
      while (message.isEmpty) {
        println("Before")
        Thread.sleep(1000)
        println("After")
        lock.wait()
      }
      println(message.get)
    }
  }
  Thread.sleep(2000)
  lock.synchronized {
    message = Some("Hello!")
    println("Sleeping")
    Thread.sleep(3000)
    lock.notify()
  }
  greeter.join()
}
