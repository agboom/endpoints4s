package endpoints4s.algebra.server

import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.model.headers.`WWW-Authenticate`
import endpoints4s.algebra.AuthenticationTestApi

trait AuthenticationTestSuite[T <: AuthenticationTestApi] extends EndpointsTestSuite[T] {
  "Authentication" should {
    "reject unauthenticated requests" in {
      serveEndpoint(serverApi.basicAuthEndpoint, Right("Hello!")) { port =>
        val request = HttpRequest(uri = s"http://localhost:$port/users")
        whenReady(sendAndDecodeEntityAsText(request)) { case (response, entity) =>
          response.status shouldBe StatusCodes.Unauthorized
          println(response.headers)
          response
            .header[`WWW-Authenticate`]
            .exists(_.challenges.exists(_.scheme == "Basic")) shouldBe true
          entity shouldBe ""
          ()
        }
      }
    }
  }
}
