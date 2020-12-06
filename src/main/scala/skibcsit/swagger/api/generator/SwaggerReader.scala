package skibcsit.swagger.api.generator

import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.media.{Content, Schema}
import io.swagger.v3.oas.models.{OpenAPI, Operation, PathItem}

import scala.jdk.CollectionConverters._

object SwaggerReader {
  def read(location: String): OpenAPI = new OpenAPIParser().readLocation(location, null, null).getOpenAPI

  def getMethods(openAPI: OpenAPI): Iterable[Operation] = openAPI.getPaths.entrySet().asScala.map(_.getValue).flatMap(getOperations)

  def getOperations(pathItem: PathItem): Iterable[Operation] = List(pathItem.getGet, pathItem.getPost, pathItem.getPut, pathItem.getDelete, pathItem.getHead, pathItem.getOptions, pathItem.getTrace, pathItem.getPatch).filter(_ != null)

  def getFirstSchema(content: Content): Schema[_] = content.entrySet.asScala.headOption.map(_.getValue.getSchema).orNull
}
