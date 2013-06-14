UNDER CONSTRUCTION
==================
This library is a custom Google Protocol Buffers serializier/parser written in Scala.

Protocol Buffers (or simply protobuf) is a popular and effective data serializiation format and language-agnostic
RPC protocol. Read more about protobuf: [official docs](https://developers.google.com/protocol-buffers/docs/overview),
[Wikipedia](http://en.wikipedia.org/wiki/Protocol_buffers).

Why another library?
--------------------
The official approach for serializing protobuf requires writing `.proto` files containing message descriptors. These
descriptors are then compiled into target language source files. Source generators are available for a variety of
languages. While this practice is useful when integrating services written in different languages and for message format
versioning, it is not convenient when you just want to serialize yor case classes into protobuf. Additionally, generated
sources are sometimes very difficult to read and understand, and contain some patterns that you cannot escape (examples
coming later).

How to use it?
--------------
Short answer: __don't use it__. It's not ready for production at the moment.

Long answer (well, not that long. More to come):
```scala
import me.alexbool.macrobuf.Protobuf

case class MyMessage(id: Int, name: Option[String], someOtherData: Seq[Long])

val serializer = Protobuf.macroSerializer[MyMessage]
val serializied = serializer.serialize(MyMessage(1, Some("You are beautiful when using this library!"), Seq(2, 3, 4))
```

[![Build Status](https://travis-ci.org/alexbool/macrobuf.png)](https://travis-ci.org/alexbool/macrobuf)