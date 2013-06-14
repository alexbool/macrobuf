package me.alexbool.macrobuf.macros

import scala.reflect.macros.Context

class ParserHelper[C <: Context](val c: C) {

  import c.universe._
}
