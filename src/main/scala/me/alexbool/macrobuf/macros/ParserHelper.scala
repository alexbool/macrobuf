package me.alexbool.macrobuf.macros

import scala.reflect.macros.Context
import me.alexbool.macrobuf.MessageMetadata
import com.google.protobuf.CodedInputStream

class ParserHelper[C <: Context](val c: C) {

  import c.universe._

  val mm = MessageMetadata[c.universe.type](c.universe)

  def readPrimitive(tpe: c.Type)(in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    import c.universe.definitions._
    if      (tpe =:= IntTpe)         readInt(in)
    else if (tpe =:= LongTpe)        readLong(in)
    else if (tpe =:= ShortTpe)       readShort(in)
    else if (tpe =:= BooleanTpe)     readBoolean(in)
    else if (tpe =:= FloatTpe)       readFloat(in)
    else if (tpe =:= DoubleTpe)      readDouble(in)
    else if (tpe =:= typeOf[String]) readString(in)
    else throw new IllegalArgumentException("Unsupported primitive type")
  }

  // Parsers for primitive types
  def readInt(in: c.Expr[CodedInputStream]): c.Expr[Int]         = reify { in.splice.readInt32() }
  def readLong(in: c.Expr[CodedInputStream]): c.Expr[Long]       = reify { in.splice.readInt64() }
  def readShort(in: c.Expr[CodedInputStream]): c.Expr[Short]     = reify { in.splice.readInt32().asInstanceOf[Short] }
  def readBoolean(in: c.Expr[CodedInputStream]): c.Expr[Boolean] = reify { in.splice.readBool() }
  def readFloat(in: c.Expr[CodedInputStream]): c.Expr[Float]     = reify { in.splice.readFloat() }
  def readDouble(in: c.Expr[CodedInputStream]): c.Expr[Double]   = reify { in.splice.readDouble() }
  def readString(in: c.Expr[CodedInputStream]): c.Expr[String]   = reify { in.splice.readString() }

  def parseField(f: mm.Field, in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    ???
  }

  def parseMessage(m: mm.Message, in: c.Expr[CodedInputStream]): c.Expr[Any] = {
    // 1. Declare vars
    // 2. Loop while !in.isAtEnd(): read tag, decode field number and type
    // 3. Pattern match on number
    // 4. Require wire type matches field metadata, throw exception if needed
    // 5. Read value from loop
    // 6. Go to step 2
    // 7.
    ???
  }
}
