package endpoints4s.algebra

/** A middleware that can bypass or wrap an existing endpoint definition.
  *
  * @param serverAction    The server implementation of the middleware. The server implementation
  *                        is a function that intercepts incoming requests, and that can either
  *                        bypass the wrapped endpoint (by returning a response), or invoke the
  *                        wrapped endpoint logic. In the latter case, the server implementation
  *                        provides the request value for the resulting endpoint, and a function
  *                        mapping the response of the resulting endpoint to the response of the
  *                        wrapped endpoint.
  * @param fromNewResponse Maps the response of the resulting endpoint to the response of the
  *                        wrapped endpoint.
  * @param toNewResponse   Maps the response of the wrapped endpoint to the response of the
  *                        resulting endpoint.
  * @param fromNewRequest  Maps the request of the resulting endpoint to the request of the
  *                        wrapped endpoint.
  * @tparam Request        Information carried by the request of the wrapped endpoint.
  * @tparam Response       Information carried by the response of the wrapped endpoint.
  * @tparam NewRequest     Information carried by the request of the endpoint resulting from the
  *                        application of the middleware to the wrapped endpoint.
  * @tparam NewResponse    Information carried by the response of the endpoint resulting from the
  *                        application of the middleware to the wrapped endpoint.
  * @note                  All these functions may be concurrently called multiple times by
  *                        server interpreters. They must be thread-safe.
  */
trait Middleware[Request, Response, NewRequest, NewResponse] {
  def serverAction(request: Request): Middleware.ServerAction[Response, NewRequest]
  def fromNewResponse(newResponse: NewResponse): Response
  def toNewResponse(response: Response): NewResponse
  def fromNewRequest(newRequest: NewRequest): Request
}

trait MiddlewareF[F[_], Request, Response, NewRequest, NewResponse] {
  def serverAction(request: Request): Middleware.ServerActionF[F, Response, NewRequest]
  def toNewRequest(r: F[NewRequest]): NewRequest
}

trait ShortcircuitMiddleware[E, Request, Response, NewRequest, NewResponse] {
  def serverAction(request: Request): Middleware.Conditional[E, NewRequest]

  def clientAction(newRequest: NewRequest): Middleware.Conditional[E, Request]

  def fromNewResponse(newResponse: NewResponse): Response

  def fromNewRequest(newRequest: NewRequest): Request
}

object Middleware {

  type Id[T] = T

  sealed trait ServerActionF[F[_], +Response, +NewRequest]

  case class ContinueF[F[_], NewRequest](newRequest: F[NewRequest])
      extends ServerActionF[F, Nothing, NewRequest]

  //type MiddlewareF[F[_], Request, Response, NewRequest, NewResponse] =
  //  Middleware[Request, F[Response], NewRequest]

  //type RequestServerAction[Response, NewRequest] = ServerAction[Response, NewRequest, Nothing]

  //type ResponseServerAction[Response, NewResponse] = ServerAction[Response, Nothing, NewResponse]

  /** The application of a middleware to an endpoint can either [[Bypass]] the endpoint
    * logic, or invoke its business logic (with [[Continue]]).
    *
    * @tparam Response   Information carried by the response of the wrapped endpoint.
    * @tparam NewRequest Information carried by the request of the endpoint resulting
    *                    from the application of the middleware to the wrapped endpoint.
    */
  sealed trait ServerAction2[+Response, +NewResponse, +NewRequest]

  type ServerAction[Response, NewRequest] = ServerAction2[Response, Nothing, NewRequest]

  /** Bypass the logic of an endpoint, and immediately return the given `response`.
    *
    * @param response  Response value sent by the middleware.
    * @tparam Response Information carried by the response of the wrapped endpoint.
    */
  case class Bypass[Response](response: Response) extends ServerAction[Response, Nothing]

  /** Invoke the logic of the resulting endpoint.
    *
    * @param newRequest     Request value used by the resulting endpoint logic.
    * @tparam NewRequest    Information carried by the request of the endpoint resulting
    *                       from the application of the middleware to the wrapped endpoint.
    */
  case class Continue[NewRequest](newRequest: NewRequest) extends ServerAction[Nothing, NewRequest]

  case class Conditional[LeftResponse, NewRequest](
      condition: Either[LeftResponse, NewRequest]
  ) extends ServerAction[Either[LeftResponse, NewRequest], NewRequest]

}
