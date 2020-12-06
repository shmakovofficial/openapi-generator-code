package skibcsit.swagger.api.generator

import java.io.PrintWriter
import scala.util.{Try, Using}

object SwaggerWriter {

  def write(path: String, content: String): Try[Unit] = Using(new PrintWriter(path)) { writer => writer.write(content) }

}
