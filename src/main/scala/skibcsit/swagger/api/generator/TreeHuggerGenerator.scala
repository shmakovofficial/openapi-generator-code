package skibcsit.swagger.api.generator

import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.responses.ApiResponses
import io.swagger.v3.oas.models.{OpenAPI, Operation}
import treehugger.forest._
import treehuggerDSL._

import java.util
import scala.jdk.CollectionConverters._

object TreeHuggerGenerator extends Generator {

  val CODE_200: String = "200"
  val SERVICE_NAME: String = "Service"
  val BOOLEAN: Type = TYPE_REF("Boolean")
  val INTEGER: Type = TYPE_REF("Integer")
  val LONG: Type = TYPE_REF("Long")
  val DOUBLE: Type = TYPE_REF("Double")
  val FLOAT: Type = TYPE_REF("Float")
  val STRING: Type = TYPE_REF("String")
  val ??? : Ident = REF("???")

  override def generateClasses(`package`: String, openAPI: OpenAPI): String = treeToString(generatePackageObject(`package`, openAPI))

  override def generateService(`package`: String, openAPI: OpenAPI): String = treeToString(generateServiceObject(`package`, SwaggerReader.getMethods(openAPI).map(generateMethod)))

  def generatePackageObject(`package`: String, openAPI: OpenAPI): PackageDef = PACKAGEOBJECTDEF(getPackageSuffix(`package`)).:=(BLOCK()).inPackage(`package`.substring(0, `package`.lastIndexOf('.')))

  def generateServiceObject(`package`: String, methods: Iterable[DefDef]): PackageDef = OBJECTDEF(SERVICE_NAME).:=(BLOCK(methods)).inPackage(`package`)

  def generateMethod(operation: Operation): DefDef = DEF(operation.getOperationId).withParams(generateParams(operation)).withType(generateResponseType(operation.getResponses)).:=(???)

  def generateResponseType(apiResponses: ApiResponses): Type = Option(generateDefaultType(apiResponses)).map(`type` => if (hasErrorResponse(apiResponses)) TYPE_EITHER(STRING, `type`) else `type`).getOrElse(STRING)

  def generateDefaultType(apiResponses: ApiResponses): Type = Option(apiResponses.getOrDefault(CODE_200, apiResponses.getDefault)).map(_.getContent).map(SwaggerReader.getFirstSchema).map(generateParamType).getOrElse(STRING)

  def hasErrorResponse(apiResponses: ApiResponses): Boolean = Option(apiResponses.keySet()).map(_.asScala).map(_.filterNot(_.equals("200"))).map(_.filterNot(_.equals("default"))).exists(_.nonEmpty)

  def generateParams(operation: Operation): Iterable[ValDef] = appendNotNull(fromParameters(operation.getParameters), fromRequestBody(operation.getRequestBody))

  def appendNotNull(params: Iterable[ValDef], value: ValDef): Iterable[ValDef] = if (value != null) params.concat(List(value)) else params

  def fromParameters(parameters: util.List[Parameter]): Iterable[ValDef] = Option(parameters).map(_.asScala.map(generateParam)).getOrElse(List.empty)

  def fromRequestBody(requestBody: RequestBody): ValDef = Option(requestBody).map(_.getContent).map(SwaggerReader.getFirstSchema).map(generateParamType).map(generateParam("body", _, requestBody.getRequired)).orNull

  def generateParam(parameter: Parameter): ValDef = generateParam(parameter.getName, generateParamType(parameter.getSchema), parameter.getRequired)

  def generateParam(name: String, `type`: Type, required: Boolean): ValDef = PARAM(name, wrapWithOption(`type`, required))

  def wrapWithOption(`type`: Type, required: Boolean): Type = if (required) `type` else TYPE_OPTION(`type`)

  def generateParamType(schema: Schema[_]): Type = schema match {
    case integerSchema: IntegerSchema => integerFromFormat(integerSchema.getFormat)
    case numberSchema: NumberSchema => numberFromFormat(numberSchema.getFormat)
    case _: BooleanSchema => BOOLEAN
    case _: StringSchema => STRING
    case arraySchema: ArraySchema => TYPE_ARRAY(generateParamType(arraySchema.getItems))
    case null => STRING
    case _ => typeFromSchema(schema)
  }

  def integerFromFormat(format: String): Type = format match {
    case "int64" => LONG
    case _ => INTEGER
  }

  def numberFromFormat(format: String): Type = format match {
    case "float" => FLOAT
    case _ => DOUBLE
  }

  def typeFromSchema(schema: Schema[_]): Type = if (schema.getName != null) TYPE_REF(schema.getName) else if (schema.get$ref() != null) TYPE_REF(getRefSuffix(schema.get$ref())) else STRING

  def getSuffix(separator: Char)(string: String): String = string.split(separator).last

  val getRefSuffix: String => String = getSuffix('/')
  val getPackageSuffix: String => String = getSuffix('.')

}
