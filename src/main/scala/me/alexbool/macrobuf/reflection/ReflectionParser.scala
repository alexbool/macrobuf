package me.alexbool.macrobuf.reflection

import scala.reflect.runtime.universe._
import scala.reflect.runtime.universe.definitions._
import java.io.InputStream
import com.google.protobuf.{CodedInputStream, WireFormat}
import me.alexbool.macrobuf.{MessageMetadata, Parser}
import MessageMetadata.runtime._

class ReflectionParser[T](tpe: Type) extends Parser[T] {
  private val parser = new ReflectionMessageParser(MessageMetadata.runtime(tpe))

  def parse(input: InputStream) = {
    val codedIn = CodedInputStream.newInstance(input)
    parser.parseUntilLimit(codedIn).asInstanceOf[T]
  }
}

class ListReflectionParser[T](tpe: Type) extends Parser[Seq[T]] {
  private val parser = new ReflectionMessageParser(MessageMetadata.runtime(tpe))

  def parse(input: InputStream) = {
    val codedIn = CodedInputStream.newInstance(input)
    val buffer = collection.mutable.ListBuffer[T]()
    while (!codedIn.isAtEnd) {
      buffer += parser.parseOne(codedIn).asInstanceOf[T]
    }
    buffer.to[Seq]
  }
}

private[macrobuf] class ReflectionMessageParser(message: Message) extends MessageParser {

  class FieldAndParser(val field: Field, val parser: FieldParser[Any])

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
      if      (tpe =:= IntTpe)          FieldParsers.IntParser
      else if (tpe =:= LongTpe)         FieldParsers.LongParser
      else if (tpe =:= ShortTpe)        FieldParsers.ShortParser
      else if (tpe =:= BooleanTpe)      FieldParsers.BooleanParser
      else if (tpe =:= FloatTpe)        FieldParsers.FloatParser
      else if (tpe =:= DoubleTpe)       FieldParsers.DoubleParser
      else if (tpe =:= typeOf[String])  FieldParsers.StringParser
      else throw new IllegalArgumentException("Unknown primitive type")
    parser.asInstanceOf[ScalarFieldParser[Any]]
  }

  def parseUntilLimit(in: CodedInputStream): Any = {
    val fieldValuesByNumber = collection.mutable.ArrayBuffer[(Int, Any)]()
    while (!in.isAtEnd) {
      val number = WireFormat.getTagFieldNumber(in.readTag())
      require(number > 0, "Unexpected end of input stream")
      for (value <- fieldAndParsersByNumber(number).parser.parse(in))
        fieldValuesByNumber += ((number, value))
    }
    val preparedArguments = prepareArguments(fieldValuesByNumber)
    ctorMirror(preparedArguments:_*)
  }

  private def prepareArguments(fieldValuesByNumber: Seq[(Int, Any)]): Seq[Any] = {
    val allFieldNumbers = 1 to message.fields.size
    val groupedFieldValues = fieldValuesByNumber
      .groupBy(_._1)
      .mapValues(_.map(_._2))

    allFieldNumbers
      .map(n => (n, groupedFieldValues.getOrElse(n, Seq())))
      .to[Seq]
      .sortBy(_._1)
      .map(e => {
        require(fieldAndParsersByNumber.get(e._1).isDefined, "Unknown field number")
        val field = fieldAndParsersByNumber(e._1).field
        val rawValues: Seq[Any] = e._2
        val preparedValue = field match {
          case f: Scalar if f.optional => {
            require(rawValues.size <= 1, s"Multiple values for optional field ${f.fieldName}")
            rawValues.headOption
          }
          case f: Scalar if !f.optional => {
            require(rawValues.size <= 1, s"Multiple values for scalar field ${f.fieldName}")
            require(rawValues.size  > 0, s"No value provided for scalar field ${f.fieldName}")
            rawValues.head
          }
          case f: Repeated => {
            rawValues
          }
        }
        preparedValue
      })
  }
}
