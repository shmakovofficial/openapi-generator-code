package skibcsit.swagger.api.generator

import io.swagger.v3.oas.models.OpenAPI

trait Generator {
  def generate(name: String, openAPI: OpenAPI): String
}
