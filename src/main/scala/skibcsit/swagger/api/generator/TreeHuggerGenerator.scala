package skibcsit.swagger.api.generator

import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.{OpenAPI, Operation}
import treehugger.forest._
import treehuggerDSL._

import java.util
import scala.jdk.CollectionConverters._

object TreeHuggerGenerator extends Generator {

  private final val BOOLEAN: Type = TYPE_REF("Boolean")
  private final val INTEGER: Type = TYPE_REF("Integer")
  private final val LONG: Type = TYPE_REF("Long")
  private final val DOUBLE: Type = TYPE_REF("Double")
  private final val FLOAT: Type = TYPE_REF("Float")
  private final val STRING: Type = TYPE_REF("String")
  private final val ??? : Ident = REF("???")

  override def generateClasses(`package`: String, openAPI: OpenAPI): String = treeToString(generatePackageObject(`package`, openAPI))

  private def generatePackageObject(`package`: String, openAPI: OpenAPI): PackageDef = PACKAGEOBJECTDEF(getSuffix('.')(`package`)).:=(BLOCK()).inPackage(`package`.substring(0, `package`.length - getSuffix('.')(`package`).length - 1))

  override def generateService(`package`: String, openAPI: OpenAPI): String = treeToString(generateObject(`package`, SwaggerReader.getMethods(openAPI).map(generateMethod)))

  private def generateObject(`package`: String, methods: Iterable[DefDef]): PackageDef = OBJECTDEF("Service").:=(BLOCK(methods)).inPackage(`package`)

  private def generateMethod(operation: Operation): DefDef = DEF(operation.getOperationId).withParams(generateParams(operation)).:=(???)

  private def generateParams(operation: Operation): Iterable[ValDef] = appendNotNull(fromParameters(operation.getParameters), fromRequestBody(operation.getRequestBody))

  private def appendNotNull(params: Iterable[ValDef], value: ValDef): Iterable[ValDef] = if (value != null) params.concat(List(value)) else params

  private def fromParameters(parameters: util.List[Parameter]): Iterable[ValDef] = Option(parameters).map(_.asScala.map(generateParam)).getOrElse(List.empty)

  private def fromRequestBody(requestBody: RequestBody): ValDef = Option(requestBody).map(SwaggerReader.getFirstSchema).map(generateType).map(generateParam("body", _, requestBody.getRequired)).orNull

  private def generateParam(parameter: Parameter): ValDef = generateParam(parameter.getName, generateType(parameter.getSchema), parameter.getRequired)

  private def generateParam(name: String, `type`: Type, required: Boolean): ValDef = PARAM(name, wrapWithOption(`type`, required))

  private def wrapWithOption(`type`: Type, required: Boolean): Type = if (required) `type` else TYPE_OPTION(`type`)

  private def generateType(schema: Schema[_]): Type = schema match {
    case integerSchema: IntegerSchema => integerFromFormat(integerSchema.getFormat)
    case numberSchema: NumberSchema => numberFromFormat(numberSchema.getFormat)
    case _: BooleanSchema => BOOLEAN
    case _: StringSchema => STRING
    case arraySchema: ArraySchema => TYPE_ARRAY(generateType(arraySchema.getItems))
    case _ => typeFromSchema(schema)
  }

  private def integerFromFormat(format: String): Type = format match {
    case "int64" => LONG
    case _ => INTEGER
  }

  private def numberFromFormat(format: String): Type = format match {
    case "float" => FLOAT
    case _ => DOUBLE
  }

  private def typeFromSchema(schema: Schema[_]): Type = if (schema.getName != null) TYPE_REF(schema.getName) else if (schema.get$ref() != null) TYPE_REF(getSuffix('/')(schema.get$ref())) else STRING

  private def getSuffix(c: Char)(string: String): String = if (string.contains(c)) string.substring(string.lastIndexOf(c) + 1) else string

}
