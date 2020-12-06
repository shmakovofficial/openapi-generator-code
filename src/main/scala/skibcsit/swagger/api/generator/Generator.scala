package skibcsit.swagger.api.generator

import io.swagger.v3.oas.models.OpenAPI

trait Generator {
  def generateService(`package`: String, openAPI: OpenAPI): String

  def generatePackage(`package`: String, openAPI: OpenAPI): String
}
