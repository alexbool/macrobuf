package me.alexbool.macrobuf

import scala.reflect.runtime.universe._
import java.io.InputStream
import MessageMetadata.runtime._

class ReflectionParser[T](tpe: Type) extends Parser[T] {
  def parse(input: InputStream) = ???
}

private[macrobuf] class ReflectionMessageParser(message: Message) {

}
