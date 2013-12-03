package me.alexbool.macrobuf

import scala.util.Random

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

  def serialize[T](data: Seq[T], serializer: Serializer[Iterable[T]]) = {
    val start = System.currentTimeMillis
    val serialized = serializer.serialize(data)
    val end = System.currentTimeMillis
    val duration = end - start
    println(f"Took $duration millis at ${serialized.size.toDouble / 1024 / (duration.toDouble / 1000)}%.2f k/s")
    serialized
  }

  def parse[T](data: Array[Byte], parser: Parser[Seq[T]]) {
    val start = System.currentTimeMillis
    parser.parse(data)
    val end = System.currentTimeMillis
    val duration = end - start
    println(f"Took $duration millis at ${data.size.toDouble / 1024 / (duration.toDouble / 1000)}%.2f k/s")
  }

  println("Generating data...")
  val data = generate(10000)
  println(s"Successfully generated ${data.size} items")
  println()

  val reflectionSerializer = Protobuf.listSerializer[BenchmarkMessage]
  val macroSerializer      = Protobuf.listMacroSerializer[BenchmarkMessage]
  val reflectionParser     = Protobuf.listParser[BenchmarkMessage]
  val macroParser          = Protobuf.listMacroParser[BenchmarkMessage]

  println("----Serialization----")
  println("Reflection")
  val serializedWithReflection = serialize(data, reflectionSerializer)
  println("Macro")
  val serializedWithMacro = serialize(data, macroSerializer)
  require(serializedWithReflection.deep == serializedWithMacro.deep,
    "Same data serialized by reflection serializer and macro serializer must be equal")

  println("-------Parsing-------")
  println("Reflection")
  parse(serializedWithMacro, reflectionParser)
  println("Macro")
  parse(serializedWithMacro, macroParser)
}
