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
import java.util.UUID
import cats.conversions.all
import org.http4s.server.blaze.BlazeServerBuilder

object Main extends IOApp {

  // Database
  val snjl: Movie = Movie(
    "6bcbca1e-efd3-411d-9f7c-14b872444fce",
    "Zack Snyder's Justice League",
    2021,
    List(
      "Henry Cavill",
      "Gal Godot",
      "Ezra Miller",
      "Ben Affleck",
      "Ray Fisher",
      "Jason Momoa"
    ),
    "Zack Snyder"
  )

  val movies: Map[String, Movie] = Map(snjl.id -> snjl)
  // End data base

  // Business logic
  private def findMovieById(movieId: UUID) =
    movies.get(movieId.toString)

  private def findMoviesByDirector(director: String): List[Movie] =
    movies.values.filter(_.director == director).toList

  // end business logic
  type Actor = String
  case class Movie(
      id: String,
      title: String,
      year: Int,
      actors: List[Actor],
      director: String
  )

  case class DirectorDetails(firstname: String, lastName: String)

  val directorDetailsDB: Map[Director, DirectorDetails] =
    Map(Director("Zack", "Snyder") -> DirectorDetails("Zack", "Snyder"))

  object DirectorQueryParamMatcher
      extends QueryParamDecoderMatcher[String]("director")

  implicit val yearQueryParamDecoder: QueryParamDecoder[Year] =
    QueryParamDecoder[Int].emap(yearInt =>
      Try(Year.of(yearInt)).toEither.leftMap(fail =>
        ParseFailure(s"ERROR Parsing Year", s"${fail}")
      )
    )

  object YearQueryParamMatcher
      extends OptionalValidatingQueryParamDecoderMatcher[Year]("year")

  case class Director(firstName: String, lastName: String) {
    override def toString = s"$firstName $lastName"
  }

  def movieRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl: Http4sDsl[F] = Http4sDsl[F]
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "movies" :? DirectorQueryParamMatcher(
            director
          ) +& YearQueryParamMatcher(maybeYear) =>
        val moviesByDirector = findMoviesByDirector(director)
        maybeYear match {
          case None => Ok(moviesByDirector.asJson)
          case Some(validatedYear) =>
            validatedYear.fold(
              _ => BadRequest(s"Error in Year format"),
              year => {
                val moviesByDirectorAndYear =
                  moviesByDirector.filter(_.year == year.getValue())
                Ok(moviesByDirectorAndYear.asJson)
              }
            )
        }
      case GET -> Root / "movies" / UUIDVar(movieId) / "actors" =>
        findMovieById(movieId) match {
          case None        => BadRequest("cannot fetch movie ID")
          case Some(movie) => Ok(movie.actors.asJson)
        }

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
          case None        => NotFound(s"no ${director} found")
          case Some(value) => Ok(value.asJson)
        }
    }
  }

  def allRoutes[F[_]: Monad]: HttpRoutes[F] =
    movieRoutes[F] <+> directorRoutes[F]

  def allRoutesComplete[F[_]: Monad]: HttpApp[F] =
    allRoutes[F].orNotFound

  override def run(args: List[String]): IO[ExitCode] = {
    val apis = Router(
      "/api" -> movieRoutes[IO],
      "/api/admin" -> directorRoutes[IO]
    ).orNotFound

    BlazeServerBuilder[IO](runtime.compute)
      .bindHttp(9000, "localhost")
      .withHttpApp(allRoutesComplete)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }

}
