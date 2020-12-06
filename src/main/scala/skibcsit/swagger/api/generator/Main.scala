package skibcsit.swagger.api.generator

import org.apache.commons.cli.{DefaultParser, HelpFormatter, Options}

import scala.util.{Failure, Success, Try}

object Main {
  val SERVICE_FILENAME: String = "Service.scala"
  val PACKAGE_FILENAME: String = "package.scala"
  val APP_NAME: String = "swagger-api-gen"

  val DEFAULT_INPUT_FILE: String = "https://petstore.swagger.io/v2/swagger.json"
  val DEFAULT_OUTPUT_DIRECTORY: String = "./target/"
  val DEFAULT_GENERATOR: Generator = TreeHuggerGenerator
  val DEFAULT_PACKAGE: String = "org.example.petstore"

  val INPUT_FILE_OPTION: String = "if"
  val OUTPUT_DIRECTORY_OPTION: String = "od"
  val GENERATOR_OPTION: String = "g"
  val PACKAGE_OPTION: String = "p"

  val OPTIONS: Options = new Options {
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
      case Success(commandLine) =>
        val inputFile: String = Option(commandLine.getOptionValue(INPUT_FILE_OPTION)).getOrElse(DEFAULT_INPUT_FILE)
        val outputDirectory: String = Option(commandLine.getOptionValue(OUTPUT_DIRECTORY_OPTION)).getOrElse(DEFAULT_OUTPUT_DIRECTORY)
        val generator: Generator = Option(commandLine.getOptionValue(GENERATOR_OPTION)).map(getGenerator).getOrElse(DEFAULT_GENERATOR)
        val `package`: String = Option(commandLine.getOptionValue(PACKAGE_OPTION)).getOrElse(DEFAULT_PACKAGE)
        Try(SwaggerReader.read(inputFile)) match {
          case Failure(exception) =>
            println(exception.getMessage)
          case Success(openAPI) =>
            SwaggerWriter.write(outputDirectory + SERVICE_FILENAME, generator.generateService(`package`, openAPI))
            SwaggerWriter.write(outputDirectory + PACKAGE_FILENAME, generator.generatePackage(`package`, openAPI))
        }
    }
  }

  def getGenerator(s: String): Generator = s match {
    case _ => DEFAULT_GENERATOR
  }
}
