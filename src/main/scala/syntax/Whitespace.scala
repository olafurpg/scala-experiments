package syntax

import scala.meta.Token
import scala.meta.tokens.Token.CR
import scala.meta.tokens.Token.FF
import scala.meta.tokens.Token.LF
import scala.meta.tokens.Token.Space
import scala.meta.tokens.Token.Tab

object Whitespace {
  def unapply(token: Token): Boolean = {
    import Token._
    token.is[Space] ||
    token.is[Tab] ||
    token.is[CR] ||
    token.is[LF] ||
    token.is[FF]
  }
}
