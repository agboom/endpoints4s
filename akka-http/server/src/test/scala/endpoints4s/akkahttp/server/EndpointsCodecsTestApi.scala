package endpoints4s.akkahttp.server

import endpoints4s.algebra

/* implements the endpoint using a codecs-based json handling */
class EndpointsCodecsTestApi
    extends EndpointsTestApi
    with algebra.JsonFromCodecTestApi
    with algebra.circe.JsonFromCirceCodecTestApi
    with algebra.ChunkedEntitiesTestApi
    with algebra.ChunkedJsonEntitiesTestApi
    with algebra.circe.CounterCodecCirce
    with algebra.BasicAuthenticationTestApi
    with algebra.SumTypedEntitiesTestApi
    with algebra.AuthenticationTestApi
    with JsonEntitiesFromCodecs
    with ChunkedJsonEntities
    with BasicAuthentication 
    with Authentication {

  type AccountData = String

  def extractCredentials(accountData: String): String = accountData
  
  def validateBasicCredentials(credentials: BasicCredentials): ValidatedCredentials[AccountData] = 
    Right("valid")
}
