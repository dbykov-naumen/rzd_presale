package utils

import play.api.mvc.Result

class SimpleResultWithCORSMethods(val sr: Result) {

    def allowForAllDomains: Result = sr.withHeaders(
        "Access-Control-Allow-Origin" -> "*",
        "Access-Control-Allow-Methods" -> "GET, POST, PUT, DELETE, OPTIONS",
        "Access-Control-Max-Age" -> (60 * 60 * 24).toString,
        "Access-Control-Allow-Headers" -> "Accept, Origin, Content-type, X-Json, X-Prototype-Version, X-Requested-With",
        "Access-Control-Allow-Credentials" -> "false")


}

trait CORSHelper {
    implicit def addCORSMethodsToSimpleRequest(sr: Result): SimpleResultWithCORSMethods = new SimpleResultWithCORSMethods(sr)
}