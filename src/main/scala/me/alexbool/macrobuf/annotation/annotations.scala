package me.alexbool.macrobuf.annotation

import scala.annotation.StaticAnnotation
import scala.annotation.meta.getter

@getter
final class packed(val value: Boolean = true) extends StaticAnnotation
