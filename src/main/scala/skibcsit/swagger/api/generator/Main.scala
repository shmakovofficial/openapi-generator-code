package skibcsit.swagger.api.generator

object Main {
  final val INPUT_FILE: String = "https://petstore.swagger.io/v2/swagger.json"
  final val OUTPUT_FILE: String = "./target/output.scala"
  final val NAME: String = "MyApi"
  final val GENERATOR: Generator = TreeHuggerGenerator

  def main(args: Array[String]): Unit = {
    SwaggerWriter.write(OUTPUT_FILE, GENERATOR.generate(NAME, SwaggerReader.read(INPUT_FILE)))
  }
}
