package me.alexbool.macrobuf.macros

import scala.reflect.macros.WhiteboxMacro
import me.alexbool.macrobuf._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

trait Macros extends WhiteboxMacro {

  import c.universe._

  def serializer[T: c.WeakTypeTag]: c.Expr[Serializer[T]] = {
    val tt = implicitly[c.WeakTypeTag[T]]
    val helper = new SerializierHelper[c.type](c)
    val rm: helper.mm.RootMessage = helper.mm.apply(tt.tpe)

    val fs = c.Expr(
      Block(helper.serializeMessage(rm, c.Expr[T](Ident(TermName("obj"))),
        c.Expr[CodedOutputStream](Ident(TermName("output"))))
        .map(_.tree),
        Literal(Constant(()))))

    val resultingSerializer = reify {
      new Serializer[T] {
        def serialize(obj: T, output: CodedOutputStream) {
          fs.splice
          output.flush()
        }
      }
    }
    resultingSerializer
  }

  def listSerializer[T: c.WeakTypeTag]: c.Expr[Serializer[Iterable[T]]] = {
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
        protected def doSerialize(obj: T, output: CodedOutputStream) {
          fs.splice
        }

        protected def size(obj: T): Int = ms.splice
      }
    }
    resultingSerializer
  }

  def parser[T: c.WeakTypeTag]: c.Expr[Parser[T]] = {
    val tt = implicitly[c.WeakTypeTag[T]]
    val helper = new ParserHelper[c.type](c)
    val rm: helper.mm.RootMessage = helper.mm.apply(tt.tpe)
    val parseExpr = helper.parseMessage[T](rm, c.Expr[CodedInputStream](Ident(TermName("input"))))

    val resultingParser = reify {
      new Parser[T] {
        def parse(input: CodedInputStream) = {
          parseExpr.splice
        }
      }
    }
    resultingParser
  }

  def listParser[T: c.WeakTypeTag]: c.Expr[Parser[Seq[T]]] = {
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
