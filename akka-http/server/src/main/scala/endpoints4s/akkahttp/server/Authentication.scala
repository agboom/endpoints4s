package endpoints4s.akkahttp.server

import akka.http.scaladsl.model.headers.{
  Authorization,
  BasicHttpCredentials,
  HttpChallenges,
}
import akka.http.scaladsl.model.{HttpHeader, HttpResponse, Uri, StatusCodes => AkkaStatusCodes}
import akka.http.scaladsl.server.{Directive1, Directives}

import endpoints4s._
import akka.http.scaladsl.server.StandardRoute

trait Authentication extends algebra.Authentication {
  this: EndpointsWithCustomErrors =>

  type BasicCredentials = BasicHttpCredentials

  def decodeBasicCredentials(credentials: String): Validated[BasicCredentials] =
    Authorization.parseFromValueString(credentials) match {
      case Right(Authorization(basic: BasicHttpCredentials)) =>
        Valid(basic)
      case Right(Authorization(other)) =>
        Invalid("TODO")
      case Left(errors) =>
        Invalid(errors.map(_.summary))
    }

  override def handleServerError(throwable: Throwable): StandardRoute = {
    throwable.printStackTrace()
    StandardRoute(serverErrorResponse(throwableToServerError(throwable)))
  }
}
