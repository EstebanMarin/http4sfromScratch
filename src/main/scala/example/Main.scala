package example

import cats._
import cats.effect._
import cats.implicits._
import org.http4s.circe._
import org.http4s._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.dsl._
import org.http4s.dsl.impl._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.server._
import java.time.Year
import scala.util.Try

object Main extends App {
  type Actor = String
  case class Movie(
      id: String,
      title: String,
      year: Int,
      actors: List[Actor],
      director: StringBuilder
  )

  case class DirectorDetails(firstname: String, lastName: String)

  val directorDetailsDB: Map[Director, DirectorDetails] =
    Map(Director("Zack", "Snider") -> DirectorDetails("Zack", "Snider"))

  object DirectorQueryParamMatcher
      extends QueryParamDecoderMatcher[String]("director")

  implicit val yearQueryParamDecoder: QueryParamDecoder[Year] =
    QueryParamDecoder[Int].map(Year.of(_))

  object YearQueryParamMatcher
      extends OptionalQueryParamDecoderMatcher[Year]("year")

  case class Director(firstName: String, lastName: String) {
    override def toString = s"$firstName $lastName"
  }

  def movieRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl: Http4sDsl[F] = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "movies" :? DirectorQueryParamMatcher(
            director
          ) +& YearQueryParamMatcher(year) =>
        ???
      case GET -> Root / "movies" / UUIDVar(movieId) / "actors" => ???
    }
  }

  object DirectorPath {
    def unapply(str: String): Option[Director] =
      Try {
        val tokens = str.split(" ")
        Director(tokens(0), tokens(1))
      }.toOption
  }

  def directorRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl: Http4sDsl[F] = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "directors" / DirectorPath(director: Director) =>
        directorDetailsDB.get(director) match {
          case None => NotFound(s"no ${director} found")
          case Some(value) => Ok(value.asJson)
        }
    }
  }

  def allRoutest[F[_]: Monad]: HttpRoutes[F] =
    movieRoutes[F] <+> directorRoutes[F]

  def allRoutesComplete[F[_]: Monad]: HttpApp[F] =
    ???

}
