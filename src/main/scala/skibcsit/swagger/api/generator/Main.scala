package skibcsit.swagger.api.generator

import io.swagger.v3.oas.models.OpenAPI

object Main {
  final val INPUT_FILE: String = "https://petstore.swagger.io/v2/swagger.json"
  final val OUTPUT_DIRECTORY: String = "./target/"
  final val GENERATOR: Generator = TreeHuggerGenerator
  final val PACKAGE: String = "org.example.petstore"

  def main(args: Array[String]): Unit = {
    val openAPI: OpenAPI = SwaggerReader.read(INPUT_FILE)
    SwaggerWriter.write(OUTPUT_DIRECTORY + "Service.scala", GENERATOR.generateService(PACKAGE, openAPI))
    SwaggerWriter.write(OUTPUT_DIRECTORY + "package", GENERATOR.generateClasses(PACKAGE, openAPI))
  }
}
