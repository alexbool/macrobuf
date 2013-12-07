package me.alexbool.macrobuf.macros

import me.alexbool.macrobuf._
import Messages._

class MacroParserSpec extends ParserSpec {
  def name = "Macro protobuf parser"
  def parserForMessage1 = Protobuf.macroParser[Message1]
  def parserForMessage2 = Protobuf.macroParser[Message2]
  def parserForMessage3 = Protobuf.macroParser[Message3]
  def parserForMessage4 = Protobuf.macroParser[Message4]
  def parserForMessage5 = Protobuf.macroParser[Message5]
  def parserForMessage6 = Protobuf.macroParser[Message6]
  def parserForMessage7 = Protobuf.macroParser[Message7]
  def parserForMessage8 = Protobuf.macroParser[Message8]
  def parserForMessage9 = Protobuf.macroParser[Message9]
}
