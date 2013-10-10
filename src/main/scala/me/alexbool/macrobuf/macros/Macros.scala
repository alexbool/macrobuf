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
      Block(helper.serializeMessage(rm, c.Expr[T](Ident(TermName("obj"))),
        c.Expr[CodedOutputStream](Ident(TermName("output"))))
        .map(_.tree),
        Literal(Constant(()))))

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
      Block(helper.serializeMessage(rm, c.Expr[T](Ident(TermName("obj"))),
        c.Expr[CodedOutputStream](Ident(TermName("output"))))
        .map(_.tree),
        Literal(Constant(()))))
    val ms = helper.messageSize(rm, c.Expr[T](Ident(TermName("obj"))))

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
    val parseExpr = helper.parseMessage[T](rm, c.Expr[CodedInputStream](Ident(TermName("input"))))

    val resultingParser = reify {
      new MacroParserBase[T] {
        protected def parse(input: CodedInputStream) = {
          parseExpr.splice
        }
      }
    }
    resultingParser
  }

  def listParser[T: c.WeakTypeTag](c: Context): c.Expr[Parser[Seq[T]]] = {
    import c.universe._

    val tt = implicitly[c.WeakTypeTag[T]]
    val helper = new ParserHelper[c.type](c)
    val rm: helper.mm.RootMessage = helper.mm.apply(tt.tpe)
    val parseExpr = helper.parseDelimited[T](rm, c.Expr[CodedInputStream](Ident(TermName("input"))))

    val resultingParser = reify {
      new ListMacroParserBase[T] {
        protected def parseLengthDelimited(input: CodedInputStream) = {
          parseExpr.splice
        }
      }
    }
    resultingParser
  }
}
