import scala.util.parsing.combinator._

// ######## AST #########
abstract class LTerm

case class LVariable(c: Char) extends LTerm { override def toString() = { c.toString() }}
case class LAbstraction(v: LVariable, m: LTerm) extends LTerm { override def toString() = { "λ" + v + "." + m.toString() }}
case class LApplication(m: LTerm, n: LTerm) extends LTerm { override def toString() = { "(" + m.toString() + " " + n.toString() +")" }}


// ######## Parser #########
object LambdaParsers extends JavaTokenParsers {
  def term: Parser[LTerm] = variable | abstraction | application

  def variable: Parser[LVariable] = """[a-z]""".r ^^ {s => (LVariable(s.head))}

  def abstraction: Parser[LAbstraction] = "L"~>variable~"."~term ^^ {
    case v~"."~m => LAbstraction(v, m)
  }

  def application: Parser[LApplication] = "("~>term~term<~")" ^^ {
    case m~n => LApplication(m, n)
  }

  def parseExpr(s: String) = parse(term, s).get
}


// ######### functions ##########
def free_vars(t: LTerm) : Set[LVariable] = t match {
  case LVariable(c) => Set(LVariable(c))
  case LAbstraction(v, m) => free_vars(m) -- Set(v)
  case LApplication(m, n) => free_vars(m) ++ free_vars(n)
}

def bound_vars(t: LTerm) : Set[LVariable] = t match {
  case LVariable(_) => Set()
  case LAbstraction(v, m) => Set(v) ++ bound_vars(m)
  case LApplication(m, n) => bound_vars(m) ++ bound_vars(n)
}

def fresh() : LVariable = {
  // TODO: implement this
  LVariable('f')
}

def substitute(m: LTerm, x: LVariable, n: LTerm) : LTerm = m match {
  case LVariable(_) => {
    if (m == x)
      n
    else
      m
  }
  case LAbstraction(v, p) => {
    if (v == x)
      m
    else if (!free_vars(p).contains(x) || !free_vars(n).contains(v))
      LAbstraction(v, substitute(p, x, n))
    else if (free_vars(p).contains(x) && free_vars(n).contains(v)) {
      val z = fresh()
      substitute(LAbstraction(z, substitute(p, v, z)), x, n)
    } else {
      throw new IllegalArgumentException()
    }
  }
  case LApplication(p, q) => LApplication(substitute(p, x, n), substitute(q, x, n))
}

def reduce(m: LTerm) : LTerm = m match {
  case LApplication(LAbstraction(v, p), n) => substitute(p, v, n)
  case LVariable(_) => m
  case LAbstraction(v, m) => LAbstraction(v, reduce(m))
  case LApplication(m, n) => {
    val red_m = reduce(m)

    // Only reduce n if there was no redex in m
    if (red_m != m)
      LApplication(red_m, n)
    else
      LApplication(m, reduce(n))
  }
}

val t = LambdaParsers.parseExpr("((Lx.x y) (Lx.x y))")
println(reduce(t))
