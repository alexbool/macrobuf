package me.alexbool.macrobuf.reflection

import me.alexbool.macrobuf._
import Messages._

class ReflectionRuntimeSerializerSpec extends SerializerSpec {
  def name = "Reflection runtime protobuf serializer"
  def serializerForMessage1 = Protobuf.serializerFromClass(classOf[Message1])
  def serializerForMessage2 = Protobuf.serializerFromClass(classOf[Message2])
  def serializerForMessage3 = Protobuf.serializerFromClass(classOf[Message3])
  def serializerForMessage4 = Protobuf.serializerFromClass(classOf[Message4])
  def serializerForMessage5 = Protobuf.serializerFromClass(classOf[Message5])
  def serializerForMessage6 = Protobuf.serializerFromClass(classOf[Message6])
  def serializerForMessage7 = Protobuf.serializerFromClass(classOf[Message7])
  def serializerForMessage8 = Protobuf.serializerFromClass(classOf[Message8])
  def serializerForMessage9 = Protobuf.serializerFromClass(classOf[Message9])
  def listSerializerForMessage1 = Protobuf.listSerializerFromClass(classOf[Message1])
}
