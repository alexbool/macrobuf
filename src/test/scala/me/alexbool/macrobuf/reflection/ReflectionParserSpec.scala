package me.alexbool.macrobuf.reflection

import me.alexbool.macrobuf._
import me.alexbool.macrobuf.Message6
import me.alexbool.macrobuf.Message2
import me.alexbool.macrobuf.Message3
import me.alexbool.macrobuf.Message7
import me.alexbool.macrobuf.Message4
import me.alexbool.macrobuf.Message1
import me.alexbool.macrobuf.Message5

class ReflectionParserSpec extends ParserSpec {
  def name = "Reflection protobuf parser"
  def parserForMessage1 = Protobuf.parser[Message1]
  def parserForMessage2 = Protobuf.parser[Message2]
  def parserForMessage3 = Protobuf.parser[Message3]
  def parserForMessage4 = Protobuf.parser[Message4]
  def parserForMessage5 = Protobuf.parser[Message5]
  def parserForMessage6 = Protobuf.parser[Message6]
  def parserForMessage7 = Protobuf.parser[Message7]
  def parserForMessage8 = Protobuf.parser[Message8]
  def listParserForMessage1 = Protobuf.listParser[Message1]
}
