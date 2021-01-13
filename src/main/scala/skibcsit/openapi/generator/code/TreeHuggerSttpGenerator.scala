package skibcsit.openapi.generator.code

import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.responses.ApiResponses
import io.swagger.v3.oas.models.{OpenAPI, Operation}
import treehugger.forest
import treehugger.forest._
import treehuggerDSL._

import java.util
import scala.jdk.CollectionConverters._

object TreeHuggerSttpGenerator extends Generator {

  val CODE_200: String = "200"
  val CODE_DEFAULT: String = "default"
  val BOOLEAN: Type = TYPE_REF("Boolean")
  val INTEGER: Type = TYPE_REF("Integer")
  val LONG: Type = TYPE_REF("Long")
  val DOUBLE: Type = TYPE_REF("Double")
  val FLOAT: Type = TYPE_REF("Float")
  val STRING: Type = TYPE_REF("String")
  val BYTE_ARRAY_INPUT_STREAM: Type = TYPE_ARRAY(TYPE_REF("Byte"))
  val BACKEND: Type = TYPE_REF("SttpBackend[Identity, Nothing, NothingT]")
  val getRefSuffix: String => String = getSuffix('/')
  val getPackageSuffix: String => String = getSuffix('.')

  def getSuffix(separator: Char)(string: String): String =
    string.split(separator).last

  override def generatePackage(`package`: String, path: String, openAPI: OpenAPI): String =
    treeToString(generatePackageObject(`package`, openAPI))

  def generatePackageObject(`package`: String, openAPI: OpenAPI): PackageDef =
    PACKAGEOBJECTDEF(getPackageSuffix(`package`))
      .:=(BLOCK(generateClasses(openAPI)))
      .inPackage(`package`.substring(0, `package`.lastIndexOf('.')))

  def generateClasses(openAPI: OpenAPI): Iterable[DefTree] =
    Option(Option(openAPI.getComponents).map(_.getSchemas).orNull)
      .map(_.asScala.map((generateClass _).tupled))
      .getOrElse(List.empty)

  def generateClass(name: String, schema: Schema[_]): DefTree =
    schema match {
      case objectSchema: ObjectSchema => CASECLASSDEF(name).withParams(generateProperties(withRequired(objectSchema.getProperties, objectSchema.getRequired)))
      case _ => TYPEVAR(name).:=(STRING)
    }

  def generateProperties(properties: Iterable[(String, Schema[_], Boolean)]): Iterable[ValDef] =
    properties.map(tuple => generateParam(tuple._1, generateParamType(tuple._2), tuple._3))

  def withRequired(properties: util.Map[String, Schema[_]], required: util.List[String]): Iterable[(String, Schema[_], Boolean)] =
    Option(properties)
      .map(_.asScala)
      .map(_.map(tuple => (tuple._1, tuple._2, required != null && required.contains(tuple._1))))
      .getOrElse(List.empty)

  override def generateService(`package`: String, path: String, openAPI: OpenAPI): String =
    treeToString(generateServiceClass(`package`, path, OpenAPIReader.getMethods(openAPI).map(generateMethod)))

  def generateServiceClass(`package`: String, path: String, methods: Iterable[DefDef]): PackageDef = {
    CLASSDEF(Main.SERVICE_NAME)
      .withParams(PARAM("backend", BACKEND))
      .withAnnots(ANNOT("SttpImplementation", LIT(path)))
      .:=(BLOCK(methods.prepended(generateSttpImplicitBackend())))
      .inPackage(`package`)
  }

  def generateSttpImplicitBackend(): forest.ValDef =
    VAL("implicitBackend", BACKEND)
      .withFlags(Flags.IMPLICIT)
      .:=(REF("backend"))

  def generateMethod(operation: Operation): DefDef =
    DEF(operation.getOperationId)
      .withParams(generateParams(operation))
      .withType(generateResponseType(operation.getResponses))

  def generateParams(operation: Operation): Iterable[ValDef] =
    appendNotNull(fromParameters(operation.getParameters), fromRequestBody(operation.getRequestBody))

  def appendNotNull(params: Iterable[ValDef], value: ValDef): Iterable[ValDef] =
    if (value != null) params.concat(List(value))
    else params

  def fromParameters(parameters: util.List[Parameter]): Iterable[ValDef] =
    Option(parameters)
      .map(_.asScala.map(generateParam))
      .getOrElse(List.empty)

  def generateParam(name: String, `type`: Type, required: Boolean): ValDef =
    PARAM(name, wrapWithOption(`type`, required))

  def wrapWithOption(`type`: Type, required: Boolean): Type =
    if (required) `type`
    else TYPE_OPTION(`type`)

  def fromRequestBody(requestBody: RequestBody): ValDef =
    Option(requestBody).map(_.getContent)
      .map(OpenAPIReader.getFirstSchema)
      .map(generateParamType)
      .map(generateParam("body", _, requestBody.getRequired))
      .orNull

  def generateParam(parameter: Parameter): ValDef =
    generateParam(parameter.getName, generateParamType(parameter.getSchema), parameter.getRequired)

  def generateParamType(schema: Schema[_]): Type =
    schema match {
      case integerSchema: IntegerSchema => integerFromFormat(integerSchema.getFormat)
      case numberSchema: NumberSchema => numberFromFormat(numberSchema.getFormat)
      case _: BooleanSchema => BOOLEAN
      case _: StringSchema => STRING
      case _: FileSchema => BYTE_ARRAY_INPUT_STREAM
      case _: ByteArraySchema => BYTE_ARRAY_INPUT_STREAM
      case arraySchema: ArraySchema => TYPE_ARRAY(generateParamType(arraySchema.getItems))
      case null => STRING
      case _ => typeFromSchema(schema)
    }

  def integerFromFormat(format: String): Type =
    format match {
      case "int64" => LONG
      case _ => INTEGER
    }

  def numberFromFormat(format: String): Type =
    format match {
      case "float" => FLOAT
      case _ => DOUBLE
    }

  def typeFromSchema(schema: Schema[_]): Type =
    if (schema.getName != null) TYPE_REF(schema.getName)
    else if (schema.get$ref() != null) TYPE_REF(getRefSuffix(schema.get$ref()))
    else if (schema.getProperties != null && !schema.getProperties.isEmpty) generateTupleType(schema)
    else STRING

  def generateTupleType(schema: Schema[_]): Type =
    TYPE_TUPLE(schema.getProperties.asScala.values.map(generateParamType))

  def generateResponseType(apiResponses: ApiResponses): Type =
    Option(generateDefaultType(apiResponses))
      .map(`type` =>
        if (hasErrorResponse(apiResponses)) TYPE_EITHER(STRING, `type`)
        else `type`)
      .getOrElse(STRING)

  def generateDefaultType(apiResponses: ApiResponses): Type =
    Option(apiResponses.getOrDefault(CODE_200, apiResponses.getDefault))
      .map(_.getContent)
      .map(OpenAPIReader.getFirstSchema)
      .map(generateParamType)
      .getOrElse(STRING)

  def hasErrorResponse(apiResponses: ApiResponses): Boolean =
    Option(apiResponses.keySet())
      .map(_.asScala)
      .map(_.filterNot(_.equals(CODE_200)))
      .map(_.filterNot(_.equals(CODE_DEFAULT)))
      .exists(_.nonEmpty)

}
