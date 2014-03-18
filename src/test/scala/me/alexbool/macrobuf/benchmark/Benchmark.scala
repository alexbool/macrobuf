package me.alexbool.macrobuf.benchmark

import scala.util.Random
import scala.collection.JavaConversions._
import me.alexbool.macrobuf.{Protobuf, Parser, Serializer}

object Benchmark extends App {

  case class BenchmarkMessage(number: Int, optional: Option[Long], repeated: Seq[String], embedded: Seq[Embedded])
  case class Embedded(fieldOne: Int, fieldTwo: Long)

  def generate(count: Int): Seq[BenchmarkMessage] = Stream.continually(generate).take(count).to[Seq]

  def generate =
    BenchmarkMessage(
      number = Random.nextInt(),
      optional = if (Random.nextBoolean()) Some(Random.nextLong()) else None,
      repeated = Stream.continually(Random.alphanumeric.take(32).mkString).take(Random.nextInt(100)).to[Seq],
      embedded = Stream.continually(Embedded(fieldOne = Random.nextInt(), fieldTwo = Random.nextLong())).take(Random.nextInt(100)).to[Seq])

  def serialize[T](data: Seq[T], serializer: Serializer[T]) = {
    val start = System.currentTimeMillis
    val serialized = serializer.serializeDelimited(data)
    val end = System.currentTimeMillis
    val duration = end - start
    println(f"Took $duration millis at ${serialized.size.toDouble / 1024 / (duration.toDouble / 1000)}%.2f k/s")
    doGcWithoutWarnings()
    serialized
  }

  def parse[T](data: Array[Byte], parser: Parser[T]): Seq[T] = {
    val start = System.currentTimeMillis
    val result = parser.parseDelimited(data)
    val end = System.currentTimeMillis
    val duration = end - start
    println(f"Took $duration millis at ${data.size.toDouble / 1024 / (duration.toDouble / 1000)}%.2f k/s")
    doGcWithoutWarnings()
    result
  }

  def doGcWithoutWarnings() {
    GarbageCollectorLogger.disableGcLogging()
    System.gc()
    GarbageCollectorLogger.enableGcLoggingToStdout()
  }

  println("Generating data...")
  val data = generate(10000)
  println(s"Successfully generated ${data.size} items")
  println()

  val reflectionSerializer   = Protobuf.serializer[BenchmarkMessage]
  val macroSerializer        = Protobuf.macroSerializer[BenchmarkMessage]
  val reflectionParser       = Protobuf.parser[BenchmarkMessage]
  val macroParser            = Protobuf.macroParser[BenchmarkMessage]
  val googleParserSerializer = new GoogleCompiledParserSerializer[BenchmarkMessage, Messages.BenchmarkMessage](
    Messages.BenchmarkMessage.PARSER,
    msg => BenchmarkMessage(
      msg.getNumber,
      if (msg.hasOptional) Some(msg.getOptional) else None,
      msg.getRepeatedList,
      msg.getEmbeddedList.map(e => Embedded(e.getFieldOne, e.getFieldTwo))
    ),
    t => {
      val builder = Messages.BenchmarkMessage.newBuilder()
        .setNumber(t.number)
        .addAllRepeated(t.repeated)
        .addAllEmbedded(t.embedded.map(
          e => Messages.Embedded.newBuilder()
            .setFieldOne(e.fieldOne)
            .setFieldTwo(e.fieldTwo)
            .build()
          )
        )
      if (t.optional.isDefined) builder.setOptional(t.optional.get)
      builder.build()
    }
  )

  System.gc()
  GarbageCollectorLogger.enableGcLoggingToStdout()

  println("----Serialization----")
  println("Reflection")
  val serializedWithReflection = serialize(data, reflectionSerializer)
  println("Macro")
  val serializedWithMacro = serialize(data, macroSerializer)
  println("Google Protobuf")
  val serializedWithGoogle = serialize(data, googleParserSerializer)
  require(serializedWithReflection.deep == serializedWithMacro.deep,
    "Same data serialized by reflection serializer and macro serializer must be equal")
  require(serializedWithReflection.deep == serializedWithGoogle.deep,
    "Same data serialized by scala serializers and google serializer must be equal")

  println("-------Parsing-------")
  println("Reflection")
  val parsedByReflection = parse(serializedWithMacro, reflectionParser)
  require(data == parsedByReflection, "Data parsed by reflection parser must be the same as generated")
  println("Macro")
  val parsedByMacro = parse(serializedWithMacro, macroParser)
  require(data == parsedByMacro, "Data parsed by macro parser must be the same as generated")
  println("Google Protobuf")
  val parsedByGoogle = parse(serializedWithMacro, googleParserSerializer)
  require(data == parsedByGoogle, "Data parsed by google parser must be the same as generated")
}
