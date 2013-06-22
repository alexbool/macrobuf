package me.alexbool.macrobuf

import me.alexbool.macrobuf.annotation.packed

object Messages {
  case class Message1(number: Int)
  case class Message2(text: String)
  case class Message3(number: Option[Int])
  case class Message4(numbers: Iterable[Int])
  case class Message5(msg: Message1)
  case class Message6(msgs: Seq[Message1])
  case class Message7(msg: Option[Message1])
  case class Message8(number: Long, string: String)
  case class Message9(number: Int, @packed numbers: Iterable[Long])
}
