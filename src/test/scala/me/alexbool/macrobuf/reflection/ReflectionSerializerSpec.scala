package me.alexbool.macrobuf.reflection

import me.alexbool.macrobuf._
import Messages._

class ReflectionSerializerSpec extends SerializerSpec {
  def name = "Reflection protobuf serializer"
  def serializerForMessage1 = Protobuf.serializer[Message1]
  def serializerForMessage2 = Protobuf.serializer[Message2]
  def serializerForMessage3 = Protobuf.serializer[Message3]
  def serializerForMessage4 = Protobuf.serializer[Message4]
  def serializerForMessage5 = Protobuf.serializer[Message5]
  def serializerForMessage6 = Protobuf.serializer[Message6]
  def serializerForMessage7 = Protobuf.serializer[Message7]
  def serializerForMessage8 = Protobuf.serializer[Message8]
  def serializerForMessage9 = Protobuf.serializer[Message9]
}
