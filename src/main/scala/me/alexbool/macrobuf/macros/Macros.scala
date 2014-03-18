package me.alexbool.macrobuf.macros

import scala.reflect.macros.whitebox.Context
import me.alexbool.macrobuf._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

class Macros(val c: Context) {

  import c.universe._

  def serializer[T: c.WeakTypeTag]: c.Expr[Serializer[T]] = {
    val tt = implicitly[c.WeakTypeTag[T]]
    val helper = new SerializierHelper[c.type](c)
    val rm: helper.mm.RootMessage = helper.mm.apply(tt.tpe)

    val fs = c.Expr(
      Block(
        helper.serializeMessage(
          rm,
          c.Expr[T](Ident(TermName("obj"))),
          c.Expr[CodedOutputStream](Ident(TermName("output")))
        ).map(_.tree),
      Literal(Constant(())))
    )
    val ms = helper.messageSize(rm, c.Expr[T](Ident(TermName("obj"))))

    val resultingSerializer = reify {
      new Serializer[T] {
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
        protected def doParseUntilLimit(input: CodedInputStream) = {
          parseExpr.splice
        }
      }
    }
    System.out.println(showCode(resultingParser.tree))
    resultingParser
  }
}
