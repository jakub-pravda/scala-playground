//> using dep dev.zio::zio:2.1.19
//> using dep dev.zio::zio-streams:2.1.19

package io.jacfal.effects

import zio._
import zio.ZIOAppDefault

case class User(name: String, email: String)

class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
  def subscribeUser(user: User): Task[Unit] = { // Task = ZIO[Any, Throwable, A]
    for {
      _ <- emailService.email(user)
      _ <- userDatabase.insert(user)
    } yield()
  }
}

object UserSubscription {
  def create(emailService: EmailService, userDatabase: UserDatabase) =  new UserSubscription(emailService, userDatabase)
}

class EmailService {
  def email(user: User): Task[Unit] = ZIO.succeed(println(s"Subsctiption complete! Welcome $user"))
}

object EmailService {
  def create() = new EmailService()
}

class UserDatabase(conn: ConnectionPool) {
  def insert(user: User): Task[Unit] = for {
    connection <- conn.get
    _ <- connection.runQuery(s"INSERT $user TO DATABASE")
  } yield ()
}

object UserDatabase {
  def create(nConnections: Int) = {
    val connPool = new ConnectionPool(nConnections)
    new UserDatabase(connPool)
  }
  
  def createFromConnectionPool(connPool: ConnectionPool) = new UserDatabase(connPool)
}

class ConnectionPool(nConnections: Int) {
  def get: Task[Connection] = ZIO.succeed("Connection acquired") *> ZIO.succeed(Connection())
}
case class Connection() {
  def runQuery(q: String): Task[Unit] = ZIO.succeed(println(s"Executing query: $q"))
}

object ZIODependencies extends ZIOAppDefault {
  // ZIO Dependency Injection Way
  def subscribe(user: User) = for {
    sub <- ZIO.service[UserSubscription] // return Nothing or registered subscription
    _ <- sub.subscribeUser(user)
  } yield()
  
  val appSubscribe = for {
    _ <- subscribe(User("jacob", "jacob@email.com"))
    _ <- subscribe(User("barca", "barca@email.com"))  
    } yield()
  
  // def run = appSubscribe.provide(ZLayer.succeed(
  //     UserSubscription.create(EmailService.create(), UserDatabase.create(10))
  //   )
  // )
  
  // Zlayer[Dependency, Error channel, Value channel]
  val connectionPool: ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(new ConnectionPool(10))
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(EmailService.create())
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(UserDatabase.createFromConnectionPool(_))
  val userSubscriptionLayer: ZLayer[UserDatabase with EmailService, Nothing, UserSubscription] = ZLayer.fromFunction(UserSubscription.create(_, _))

  // layers are compostable
  // Vertical composition layer// vertical composition
  val fullDatabaseLayer: ZLayer[Any, Nothing, UserDatabase] = 
    connectionPool >>> databaseLayer
  // Horizontal composition layer 
  val subscriptionLayer: ZLayer[Any, Nothing, UserDatabase with EmailService] = fullDatabaseLayer ++ emailServiceLayer

  // mixin
  val userSubscriptionFullLayer = subscriptionLayer >>> userSubscriptionLayer
  
  // Goof aproach is to create layers in companion objects
  
  def run = appSubscribe.provide(userSubscriptionFullLayer)
}