package me.alexbool.macrobuf.annotation

import scala.annotation.StaticAnnotation
import scala.annotation.meta.getter

// scalastyle:off class.name

@getter
final class packed(val value: Boolean = true) extends StaticAnnotation

// scalastyle:on class.name
