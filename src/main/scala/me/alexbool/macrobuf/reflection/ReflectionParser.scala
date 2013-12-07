package me.alexbool.macrobuf.reflection

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import com.google.protobuf.{CodedInputStream, WireFormat}
import me.alexbool.macrobuf.{MessageMetadata, Parser}
import MessageMetadata.runtime._

class ReflectionParser[T](tpe: Type) extends Parser[T] {
  private val parser = new ReflectionMessageParser(MessageMetadata.runtime(tpe))

  protected def doParseUntilLimit(input: CodedInputStream) = {
    parser.parseUntilLimit(input).asInstanceOf[T]
  }
}

private[macrobuf] class ReflectionMessageParser(message: MessageObject) extends MessageParser {

  class FieldAndParser(val field: Field, val parser: FieldParser[Any])
  class ParsedValue(val number: Int, val value: Any)

  private val fieldAndParsersByNumber: Map[Int, FieldAndParser] =
    message.fields.map(f => (f.number, new FieldAndParser(f, parserForField(f)))).toMap

  private val ctorMirror = {
    val rm = runtimeMirror(getClass.getClassLoader)
    val cm = rm.reflectClass(rm.classSymbol(rm.runtimeClass(message.actualType)))
    cm.reflectConstructor(message.ctor)
  }

  private def parserForField(f: Field): FieldParser[Any] = f match {
    case f: RepeatedPrimitive if f.packed    => FieldParsers.packedRepeated(parserForPrimitive(f.actualType))
    case _: Primitive | _: RepeatedPrimitive => parserForPrimitive(f.actualType)
    case m: MessageField                     => new ReflectionMessageParser(m)
  }

  private def parserForPrimitive(tpe: Type): ScalarFieldParser[Any] = {
    val parser: ScalarFieldParser[_] =
      if      (tpe =:= IntTpe)          FieldParsers.intParser
      else if (tpe =:= LongTpe)         FieldParsers.longParser
      else if (tpe =:= ShortTpe)        FieldParsers.shortParser
      else if (tpe =:= BooleanTpe)      FieldParsers.booleanParser
      else if (tpe =:= FloatTpe)        FieldParsers.floatParser
      else if (tpe =:= DoubleTpe)       FieldParsers.doubleParser
      else if (tpe =:= typeOf[String])  FieldParsers.stringParser
      else throw new IllegalArgumentException("Unknown primitive type")
    parser.asInstanceOf[ScalarFieldParser[Any]]
  }

  def parseUntilLimit(in: CodedInputStream): Any = {
    val parsedValues: Seq[ParsedValue] = new Iterator[Seq[ParsedValue]] {
      def hasNext = !in.isAtEnd
      def next() = {
        val number = WireFormat.getTagFieldNumber(in.readTag())
        require(number > 0, "Unexpected end of input stream")
        val seqOfValues = fieldAndParsersByNumber(number).parser.parse(in)
        seqOfValues.map(new ParsedValue(number, _))
      }
    }.to[Seq].flatten
    val preparedArguments: Seq[Any] = prepareCtorArguments(parsedValues)
    ctorMirror(preparedArguments:_*)
  }

  private def prepareCtorArguments(fieldValuesByNumber: Seq[ParsedValue]): Seq[Any] = {
    val allFieldNumbers = 1 to message.fields.size
    val groupedFieldValues: Map[Int, Seq[Any]] =
      fieldValuesByNumber
        .groupBy(_.number)
        .mapValues(_.map(_.value))

    allFieldNumbers
      .map(n => (n, groupedFieldValues.getOrElse(n, Seq())))
      .to[Seq]
      .sortBy(_._1)
      .map(e => {
        require(fieldAndParsersByNumber.get(e._1).isDefined, "Unknown field number")
        val field = fieldAndParsersByNumber(e._1).field
        val rawValues: Seq[Any] = e._2
        val preparedValue = field match {
          case f: OptionalField => {
            require(rawValues.size <= 1, s"Multiple values for optional field ${f.fieldName}")
            rawValues.headOption
          }
          case f: RequiredField => {
            require(rawValues.size <= 1, s"Multiple values for scalar field ${f.fieldName}")
            require(rawValues.size  > 0, s"No value provided for scalar field ${f.fieldName}")
            rawValues.head
          }
          case f: RepeatedField => {
            rawValues
          }
        }
        preparedValue
      })
  }
}
