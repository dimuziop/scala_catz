package part3dataManipulation

import cats.Id

/**
 * User: pat
 * Date: 8/1/22
 * Time: 21:46
 */
object Readers {

  /*
     - configuration file => initial data structure
     - a DB layer
     - an HTTP layer
     - a business logic layer
    */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, email: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from the db table and return the status of the orderID

    def getLastOrderId(username: String): Long = 542643 // select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started") // this would start the actual server
  }

  // bootstrap
  val config = Configuration("me", "1234Me", "localhost", 1234, 8, "me@1234.com")

  // cats Reader

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn: Id[DbConnection] = dbReader.run(config)

  // Reader [I, O]
  val myOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(55))
  val myOrderStatus: Id[String] = myOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {

    val usersLastOrderIdReader: Reader[Configuration, String] = dbReader
      .map(_.getLastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    val userOrderFor: Reader[Configuration, String] = for {
      usersLastOrderIdReader <- dbReader.map(_.getLastOrderId(username))
      usersLastOrderStatusReader <- dbReader.map(_.getOrderStatus(usersLastOrderIdReader))
    } yield usersLastOrderStatusReader
    userOrderFor.run(config)
  }

  /*
    Pattern
    1. you create the initial data structure
    2. you create a reader which specifies how that data structure will be manipulated later
    3. you can then map & flatMap the reader to produce derived information
    4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }


  // TODO 1 - email a user
  def emailUser(username: String, userEmail: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.email))
    val userLastOrderStatus = getLastOrderStatus(username)
    emailServiceReader.map(
      service => service.sendEmail(userEmail, s"The user last order status is $userLastOrderStatus")
    ).run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("me"))
    println(emailUser("me", "me@me.me"))

  }

}
