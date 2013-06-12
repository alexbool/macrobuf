package me.alexbool.macrobuf

class ReflectionRuntimeProtobuferializerSpec extends ProtobufSerializerSpec {
  def name = "Reflection runtime protobuf serializer"
  def serializerForMessage1 = Protobuf.serializerFromClass(classOf[Message1])
  def serializerForMessage2 = Protobuf.serializerFromClass(classOf[Message2])
  def serializerForMessage3 = Protobuf.serializerFromClass(classOf[Message3])
  def serializerForMessage4 = Protobuf.serializerFromClass(classOf[Message4])
  def serializerForMessage5 = Protobuf.serializerFromClass(classOf[Message5])
  def serializerForMessage6 = Protobuf.serializerFromClass(classOf[Message6])
  def serializerForMessage7 = Protobuf.serializerFromClass(classOf[Message7])
  def listSerializerForMessage1 = Protobuf.listSerializerFromClass(classOf[Message1])
}
