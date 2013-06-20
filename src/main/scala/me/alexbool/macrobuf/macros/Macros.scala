package me.alexbool.macrobuf.macros

import scala.reflect.macros.Context
import me.alexbool.macrobuf._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

object Macros {

  def serializer[T: c.WeakTypeTag](c: Context): c.Expr[Serializer[T]] = {
    import c.universe._

    val tt = implicitly[c.WeakTypeTag[T]]
    val helper = new SerializierHelper[c.type](c)
    val rm: helper.mm.RootMessage = helper.mm.apply(tt.tpe)

    val fs = c.Expr(
      Block(helper.serializeMessage(rm, c.Expr[T](Ident(newTermName("obj"))),
        c.Expr[CodedOutputStream](Ident(newTermName("output"))))
        .map(_.tree),
        Literal(Constant(()))))
    println(fs)

    val resultingSerializer = reify {
      new MacroSerializerBase[T] {
        protected def serialize(obj: T, output: CodedOutputStream) {
          fs.splice
        }
      }
    }
    resultingSerializer
  }

  def listSerializer[T: c.WeakTypeTag](c: Context): c.Expr[Serializer[Iterable[T]]] = {
    import c.universe._

    val tt = implicitly[c.WeakTypeTag[T]]
    val helper = new SerializierHelper[c.type](c)
    val rm: helper.mm.RootMessage = helper.mm.apply(tt.tpe)

    val fs = c.Expr(
      Block(helper.serializeMessage(rm, c.Expr[T](Ident(newTermName("obj"))),
        c.Expr[CodedOutputStream](Ident(newTermName("output"))))
        .map(_.tree),
        Literal(Constant(()))))
    val ms = helper.messageSize(rm, c.Expr[T](Ident(newTermName("obj"))))
    println(fs)
    println(ms)

    val resultingSerializer = reify {
      new ListMacroSerializerBase[T] {
        protected def serialize(obj: T, output: CodedOutputStream) {
          fs.splice
        }

        protected def size(obj: T): Int = ms.splice
      }
    }
    resultingSerializer
  }

  def parser[T: c.WeakTypeTag](c: Context): c.Expr[Parser[T]] = {
    import c.universe._

    val tt = implicitly[c.WeakTypeTag[T]]
    val helper = new ParserHelper[c.type](c)
    val rm: helper.mm.RootMessage = helper.mm.apply(tt.tpe)
    val parseExpr = helper.parseMessage(rm, c.Expr[CodedInputStream](Ident(newTermName("input")))).asInstanceOf[c.Expr[T]]
    println(parseExpr)

    val resultingParser = reify {
      new MacroParserBase[T] {
        protected def parse(input: CodedInputStream) = {
          parseExpr.splice
        }
      }
    }
    resultingParser
  }
}
