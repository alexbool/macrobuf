package me.alexbool.macrobuf.reflection

import me.alexbool.macrobuf._
import Messages._

class ReflectionRuntimeParserSpec extends ParserSpec {
  def name = "Reflection runtime protobuf parser"
  def parserForMessage1 = Protobuf.parserFromClass(classOf[Message1])
  def parserForMessage2 = Protobuf.parserFromClass(classOf[Message2])
  def parserForMessage3 = Protobuf.parserFromClass(classOf[Message3])
  def parserForMessage4 = Protobuf.parserFromClass(classOf[Message4])
  def parserForMessage5 = Protobuf.parserFromClass(classOf[Message5])
  def parserForMessage6 = Protobuf.parserFromClass(classOf[Message6])
  def parserForMessage7 = Protobuf.parserFromClass(classOf[Message7])
  def parserForMessage8 = Protobuf.parserFromClass(classOf[Message8])
  def parserForMessage9 = Protobuf.parserFromClass(classOf[Message9])
}
