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

object Middleware {

  /** The application of a middleware to an endpoint can either [[Bypass]] the endpoint
    * logic, or invoke its business logic (with [[Continue]]).
    *
    * @tparam Response   Information carried by the response of the wrapped endpoint.
    * @tparam NewRequest Information carried by the request of the endpoint resulting
    *                    from the application of the middleware to the wrapped endpoint.
    */
  sealed trait ServerAction[+Response, +NewRequest]

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

}
