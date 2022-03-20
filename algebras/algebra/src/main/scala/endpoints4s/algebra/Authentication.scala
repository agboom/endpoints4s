package endpoints4s.algebra

import endpoints4s._

trait Authentication {
  this: EndpointsWithCustomErrors =>

  type BasicCredentials
  //type BearerCredentials

  sealed trait InvalidAuthentication extends Product with Serializable
  case object UnauthorizedError extends InvalidAuthentication
  case object ForbiddenError extends InvalidAuthentication

  type AccountData

  type ValidatedCredentials[B] = Either[InvalidAuthentication, B]

  def authenticationChallengeHeader: ResponseHeaders[Unit] =
    responseHeader("WWW-Authenticate").xmap(_ => ())(_ => """Basic, charset="UTF-8"""")

  def decodeBasicCredentials(credentials: String): Validated[BasicCredentials]

  def validateBasicCredentials(credentials: BasicCredentials): ValidatedCredentials[AccountData]

  type AuthenticationData

  def basicAuthenticationMiddleware[A, B, ACred, AAccountData](implicit
      tuplerACred: Tupler.Aux[A, Option[String], ACred],
      tuplerAAccountData: Tupler.Aux[A, AccountData, AAccountData]
  ): Middleware[ACred, ValidatedCredentials[B], AAccountData, B] =
    new Middleware[ACred, ValidatedCredentials[B], AAccountData, B] {
      def serverAction(
          request: ACred
      ): Middleware.ServerAction[ValidatedCredentials[B], AAccountData] = {
        Middleware.Continue {
          val (a, credOpt) = tuplerACred.unapply(request)
          val basic = decodeBasicCredentials(credOpt.get).toEither.toOption.get
          val accountData = validateBasicCredentials(basic).toOption.get
          tuplerAAccountData.apply(a, accountData)
        }
      }
      //  Middleware.Continue {
      //    val (a, credOpt) = tuplerACred.unapply(request)
      //    credOpt match {
      //      case Some(cred) =>
      //        decodeBasicCredentials(cred) match {
      //          case Valid(basicCredentials) =>
      //            validateBasicCredentials(basicCredentials)
      //              .map(tuplerAAccountData.apply(a, _))
      //          case Invalid(_) =>
      //            Left(unauthorized)
      //        }
      //      case None =>
      //        Left(unauthorized)
      //    }
      //  }

      def fromNewResponse(newResponse: B): ValidatedCredentials[B] = ???

      def toNewResponse(response: ValidatedCredentials[B]): B = ???

      def fromNewRequest(newRequest: AAccountData): ACred = ???
    }

  //def requestWithBasicAuthentication[A](
  //    request: Request[A]
  //)(implicit tupler: Tupler[A, BasicCredentials]): Request[tupler.Out]

  def basicAuthorizationHeader: RequestHeaders[Option[String]] =
    optRequestHeader("Authorization")

  def responseEntity[A](a: A): ResponseEntity[A] = emptyResponse.xmap(_ => a)(_ => ())

  def unauthorizedResponse: Response[UnauthorizedError.type] =
    response(
      Unauthorized,
      responseEntity(UnauthorizedError),
      None,
      authenticationChallengeHeader
    )

  def forbiddenResponse: Response[ForbiddenError.type] =
    response(
      Forbidden,
      responseEntity(ForbiddenError),
      None,
      authenticationChallengeHeader,
    )

  def authenticatedResponse[A](response: Response[A]): Response[ValidatedCredentials[A]] =
    (unauthorizedResponse orElse forbiddenResponse orElse response).xmap {
      case Left(Left(unauthorized)) => Left(unauthorized)
      case Left(Right(forbidden))   => Left(forbidden)
      case Right(a)                 => Right(a)
    } {
      case Left(UnauthorizedError) => Left(Left(UnauthorizedError))
      case Left(ForbiddenError)    => Left(Right(ForbiddenError))
      case Right(a)                => Right(a)
    }

  def endpointWithBasicAuthentication[A, B, ACred, AAccountData](
      endpoint: Endpoint[A, B]
  )(implicit
      tuplerACred: Tupler.Aux[A, Option[String], ACred],
      tuplerAAccountData: Tupler.Aux[A, AccountData, AAccountData]
  ): Endpoint[AAccountData, B] = {
    endpoint
      .mapRequest(_.addHeaders(basicAuthorizationHeader))
      .mapResponse(authenticatedResponse(_))
      .withMiddleware(basicAuthenticationMiddleware)
  }
}
