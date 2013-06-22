package me.alexbool.macrobuf.macros

import me.alexbool.macrobuf._
import Messages._

class MacroSerializerSpec extends SerializerSpec {
  def name = "Macro protobuf serializer"
  def serializerForMessage1 = Protobuf.macroSerializer[Message1]
  def serializerForMessage2 = Protobuf.macroSerializer[Message2]
  def serializerForMessage3 = Protobuf.macroSerializer[Message3]
  def serializerForMessage4 = Protobuf.macroSerializer[Message4]
  def serializerForMessage5 = Protobuf.macroSerializer[Message5]
  def serializerForMessage6 = Protobuf.macroSerializer[Message6]
  def serializerForMessage7 = Protobuf.macroSerializer[Message7]
  def serializerForMessage8 = Protobuf.macroSerializer[Message8]
  def serializerForMessage9 = Protobuf.macroSerializer[Message9]
  def listSerializerForMessage1 = Protobuf.listMacroSerializer[Message1]
}

