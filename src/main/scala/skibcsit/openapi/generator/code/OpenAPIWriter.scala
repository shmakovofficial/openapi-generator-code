package skibcsit.openapi.generator.code

import java.io.PrintWriter
import scala.util.{Try, Using}

object OpenAPIWriter {
  def write(path: String, content: String): Try[Unit] = Using(new PrintWriter(path)) { writer => writer.write(content) }
}
