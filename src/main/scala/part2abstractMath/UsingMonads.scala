package part2abstractMath

import scala.util.{Failure, Success, Try}

/**
 * User: pat
 * Date: 3/1/22
 * Time: 12:50
 */
object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  val monadList: Monad[List] = Monad[List] // fetch th implicit Monad[List]
  val aSimpleList: List[Int] = monadList.pure(2) // List(2)
  val anExtendedList: List[Int] = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // applicable to Option, Try, Future

  val aManualEither: Either[String, Int] = Right(42)

  // Either is also a monad
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr]
  val anEither: LoadingOr[Int] = loadingMonad.pure(42)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n%2 == 0) Right(n + 1) else Left("Loading a message...."))

  // imaginary online store

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yer, refreshing data....")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation: LoadingOr[String] = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // use cats extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))

  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // TODO: the service layer API of a web App
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }


  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M
    TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object OptionalHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) None
      else Some(s"request ${payload} has been accepted")
  }

  val responseOption: Option[String] = OptionalHttpService.getConnection(config).flatMap {
    connection => OptionalHttpService.issueRequest(connection, "Hello, message")
  }

  val responseOptionFor: Option[String] = for {
    conn <- OptionalHttpService.getConnection(config)
    res <- OptionalHttpService.issueRequest(conn, "Hello, for message")
  } yield res

  object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      for {
        host <- Try(cfg("host"))
        port <- Try(cfg("port"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.length < 20) Failure(new IllegalArgumentException)
      else Success(s"request ${payload} has been accepted")
  }

  val responseTry: Try[String] = TryHttpService.getConnection(config)
    .flatMap(conn => TryHttpService.issueRequest(conn, "Hello, for TRY message"))

  val responseOptionTry: Try[String] = for {
    conn <- TryHttpService.getConnection(config)
    res <- TryHttpService.issueRequest(conn, "Hello, for message")
  } yield res

  def main(args: Array[String]): Unit = {

  }

}
