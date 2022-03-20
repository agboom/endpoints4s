package endpoints4s.algebra

import endpoints4s.algebra

trait AuthenticationTestApi extends EndpointsTestApi with algebra.Authentication {

  val basicAuthEndpoint: Endpoint[AccountData, String] = endpointWithBasicAuthentication(
    endpoint(
      get(path / "users"),
      ok(textResponse)
    )
  )
}
