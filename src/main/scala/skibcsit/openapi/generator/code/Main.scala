package skibcsit.openapi.generator.code

import org.apache.commons.cli.{DefaultParser, HelpFormatter, Options}

import scala.util.{Failure, Success, Try}

object Main {
  val SERVICE_NAME: String = "Service"
  val SERVICE_FILENAME: String = SERVICE_NAME + ".scala"
  val PACKAGE_FILENAME: String = "package.scala"
  val APP_NAME: String = "openapi-generator-code"

  val DEFAULT_INPUT_FILE: String = "https://petstore.swagger.io/v2/swagger.json"
  val DEFAULT_OUTPUT_DIRECTORY: String = "./target/"
  val DEFAULT_PACKAGE: String = "org.example.petstore"
  val DEFAULT_GENERATOR_TYPE: GeneratorType = GeneratorType.TREEHUGGER
  val DEFAULT_CLIENT_TYPE: ClientType = ClientType.STTP

  val INPUT_FILE_OPTION: String = "if"
  val OUTPUT_DIRECTORY_OPTION: String = "od"
  val GENERATOR_OPTION: String = "g"
  val CLIENT_OPTION: String = "c"
  val PACKAGE_OPTION: String = "p"

  val OPTIONS: Options = new Options {
    addOption(INPUT_FILE_OPTION, "inputFile", true, "Input file")
    addOption(OUTPUT_DIRECTORY_OPTION, "outputDirectory", true, "Output directory")
    addOption(GENERATOR_OPTION, "generator", true, "Generator")
    addOption(CLIENT_OPTION, "client", true, "Client")
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
        val generatorType: GeneratorType = Option(commandLine.getOptionValue(GENERATOR_OPTION)).map(GeneratorType.valueOf).getOrElse(DEFAULT_GENERATOR_TYPE)
        val clientType: ClientType = Option(commandLine.getOptionValue(GENERATOR_OPTION)).map(ClientType.valueOf).getOrElse(DEFAULT_CLIENT_TYPE)
        val generator: Generator = getGenerator(generatorType, clientType)
        val `package`: String = Option(commandLine.getOptionValue(PACKAGE_OPTION)).getOrElse(DEFAULT_PACKAGE)
        Try(OpenAPIReader.read(inputFile)) match {
          case Failure(exception) =>
            println(exception.getMessage)
          case Success(openAPI) =>
            OpenAPIWriter.write(outputDirectory + SERVICE_FILENAME, generator.generateService(`package`, inputFile, openAPI))
            OpenAPIWriter.write(outputDirectory + PACKAGE_FILENAME, generator.generatePackage(`package`, inputFile, openAPI))
        }
    }
  }

  def getGenerator(generatorType: GeneratorType, clientType: ClientType): Generator = (generatorType, clientType) match {
    case (GeneratorType.TREEHUGGER, ClientType.STTP) => TreeHuggerSttpGenerator
    case _ => throw new IllegalArgumentException("Cannot resolve generator [" + generatorType + "] for client [" + clientType + "]")
  }
}
