package skibcsit.openapi.generator.code

import io.swagger.v3.oas.models.OpenAPI

trait Generator {
  def generateService(`package`: String, path: String, openAPI: OpenAPI): String
  def generatePackage(`package`: String, path: String, openAPI: OpenAPI): String
}
