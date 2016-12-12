package syntax

import scala.meta.Tokens
import scala.meta.Token

case class FormatToken(left: Token, right: Token, between: Vector[Token]) {

  override def toString = s"${left.syntax}∙${right.syntax}"

  def newlines: Int = between.count(_.is[Token.LF])

  def inside(range: Set[Range]): Boolean = {
    if (range.isEmpty) true
    else range.exists(_.contains(right.pos.end.line))
  }

  val leftHasNewline: Boolean = left.syntax.contains('\n')
}

object FormatToken {

  /**
    * Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as
    * little memory as possible.
    */
  def formatTokens(tokens: Tokens): Array[FormatToken] = {
    var left = tokens.head
    val result = Array.newBuilder[FormatToken]
    val whitespace = Vector.newBuilder[Token]
    tokens.toArray.foreach {
      case t @ Whitespace() => whitespace += t
      case right =>
        val tok = FormatToken(left, right, whitespace.result)
        result += tok
        left = right
        whitespace.clear()
    }
    result.result
  }
}