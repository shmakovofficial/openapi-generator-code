package skibcsit.swagger.api.generator

import io.swagger.v3.oas.models.OpenAPI
import org.apache.commons.cli.{DefaultParser, HelpFormatter, Options}

import scala.util.{Failure, Success, Try}

object Main {
  final val SERVICE_FILENAME: String = "Service.scala"
  final val PACKAGE_FILENAME: String = "package.scala"
  final val APP_NAME: String = "swagger-api-gen"

  final val DEFAULT_INPUT_FILE: String = "https://petstore.swagger.io/v2/swagger.json"
  final val DEFAULT_OUTPUT_DIRECTORY: String = "./target/"
  final val DEFAULT_GENERATOR: Generator = TreeHuggerGenerator
  final val DEFAULT_PACKAGE: String = "org.example.petstore"

  final val INPUT_FILE_OPTION: String = "if"
  final val OUTPUT_DIRECTORY_OPTION: String = "od"
  final val GENERATOR_OPTION: String = "g"
  final val PACKAGE_OPTION: String = "p"

  final val OPTIONS: Options = new Options {
    addOption(INPUT_FILE_OPTION, "inputFile", true, "Input file")
    addOption(OUTPUT_DIRECTORY_OPTION, "outputDirectory", true, "Output directory")
    addOption(GENERATOR_OPTION, "generator", true, "Generator")
    addOption(PACKAGE_OPTION, "package", true, "Package")
  }

  def main(args: Array[String]): Unit = {
    Try(new DefaultParser().parse(OPTIONS, args)) match {
      case Failure(exception) =>
        println(exception.getMessage)
        new HelpFormatter().printHelp(APP_NAME, OPTIONS)
      case Success(value) =>
        val inputFile: String = Option(value.getOptionValue(INPUT_FILE_OPTION)).getOrElse(DEFAULT_INPUT_FILE)
        val outputDirectory: String = Option(value.getOptionValue(OUTPUT_DIRECTORY_OPTION)).getOrElse(DEFAULT_OUTPUT_DIRECTORY)
        val generator: Generator = Option(value.getOptionValue(GENERATOR_OPTION)).map(getGenerator).getOrElse(DEFAULT_GENERATOR)
        val `package`: String = Option(value.getOptionValue(PACKAGE_OPTION)).getOrElse(DEFAULT_PACKAGE)
        val openAPI: OpenAPI = SwaggerReader.read(inputFile)
        SwaggerWriter.write(outputDirectory + SERVICE_FILENAME, generator.generateService(`package`, openAPI))
        SwaggerWriter.write(outputDirectory + PACKAGE_FILENAME, generator.generateClasses(`package`, openAPI))
    }
  }

  private def getGenerator(s: String): Generator = s match {
    case _ => DEFAULT_GENERATOR
  }
}
