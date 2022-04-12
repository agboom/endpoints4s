package endpoints4s.algebra

import endpoints4s._

trait Authentication {
  this: EndpointsWithCustomErrors =>

  type BasicCredentials
  //type BearerCredentials

  sealed trait InvalidAuthentication extends Product with Serializable
  case object UnauthorizedError extends InvalidAuthentication
  case object ForbiddenError extends InvalidAuthentication

  /**
   * Server: custom account data type as input for the server implementation
   *
   * Client: custom basic or bearer credential type as input for the client method
   */
  type InputCredentials

  type ValidatedCredentials[B] = Either[InvalidAuthentication, B]

  def authenticationChallengeHeader: ResponseHeaders[Unit] =
    responseHeader("WWW-Authenticate").xmap(_ => ())(_ => """Basic charset="UTF-8"""")

  def basicCredentialsCodec: Codec[String, BasicCredentials]

  def fromBasicCredentials(credentials: BasicCredentials): ValidatedCredentials[InputCredentials]

  def toBasicCredentials(inputCredentials: InputCredentials): ValidatedCredentials[BasicCredentials]

  def basicAuthenticationMiddleware[A, B, ACred, AInputCred](implicit
      tuplerACred: Tupler.Aux[A, Option[String], ACred],
      tuplerAInputCred: Tupler.Aux[A, InputCredentials, AInputCred]
  ): ShortcircuitMiddleware[InvalidAuthentication, ACred, B, AInputCred, B] =
    new ShortcircuitMiddleware[InvalidAuthentication, ACred, B, AInputCred, B] {
      def clientAction(
        request: AInputCred,
      ): Middleware.Conditional[InvalidAuthentication, ACred] =
        Middleware.Conditional {
          val (a, inputCred) = tuplerAInputCred.unapply(request)
          toBasicCredentials(inputCred)
            .map(basicCredentialsCodec.encode)
            .map(cred => tuplerACred(a, Some(cred)))
        }

      def serverAction(
          request: ACred
      ): Middleware.Conditional[InvalidAuthentication, AInputCred] =
        Middleware.Conditional {
          val (a, credOpt) = tuplerACred.unapply(request)
          credOpt match {
            case Some(cred) =>
              basicCredentialsCodec.decode(cred) match {
                case Valid(basicCredentials) =>
                  fromBasicCredentials(basicCredentials)
                    .map(tuplerAInputCred.apply(a, _))
                case Invalid(_) =>
                  Left(UnauthorizedError)
              }
            case None =>
              Left(UnauthorizedError)
          }
        }

      def fromNewRequest(newRequest: AInputCred): ACred = {
        val (a, _) = tuplerAInputCred.unapply(newRequest)
        tuplerACred(a, None) // TODO: is it necessary to add the raw input credentials here?
      }

      def fromNewResponse(newResponse: B): B = newResponse
    }

  // TODO: it would be nice to apply the basicCredentialsCodec here
  // but we'll need to be able to map Invalid to UnauthorizedError instead of BadRequest
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
      authenticationChallengeHeader
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

  def endpointWithBasicAuthentication[A, B, ACred, AInputCred](
      endpoint: Endpoint[A, B]
  )(implicit
      tuplerACred: Tupler.Aux[A, Option[String], ACred],
      tuplerAInputCred: Tupler.Aux[A, InputCredentials, AInputCred]
  ): Endpoint[AInputCred, ValidatedCredentials[B]] =
    endpointMiddlewareF(
      endpoint
        .mapRequest(_.addHeaders(basicAuthorizationHeader))
        .mapResponse(authenticatedResponse(_)),
      basicAuthenticationMiddleware
    )
}
