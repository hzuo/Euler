package hzuo.euler._17

import scala.util.matching.Regex
import scala.math.BigInt.int2bigInt

// TODO: generalize into sicp-style rule-based translator
class NumeralRead(rules0: List[(String, String)]) {

  import NumeralRead._

  private val rules = rules0
    .map { case (k, v) => (k.replaceAll("#", """\\d""").r, v) }
    .reverse

  def numeral(n: String): String =
    if (n.isEmpty) {
      ""
    } else if (n.head == '0') {
      numeral(n.tail)
    } else {
      val (pat, word) = rules.find { case (pat, _) => n.matches(pat) }.get
      n match {
        case pat(pre, rest) =>
          "%s %s %s".format(numeral(pre), word, numeral(rest)).trim
        case pat(rest) =>
          "%s %s".format(word, numeral(rest)).trim
        case _ =>
          word
      }
    }

  def apply(n: Int) =
    if (n > 0) numeral(n.toString)
    else throw new IllegalArgumentException
  def apply(n: BigInt) =
    if (n > 0) numeral(n.toString)
    else throw new IllegalArgumentException

}

object NumeralRead {

  private implicit class MatchableString(s: String) {
    def matches(pat: Regex) = pat.pattern.matcher(s).matches
  }

  def apply(rules: List[(String, String)]) = new NumeralRead(rules)

}