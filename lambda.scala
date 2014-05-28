import scala.util.parsing.combinator._

abstract class LExpression
case class LCapVarDef(c: Char, e: LTerm) extends LExpression { override def toString() = { c.toString() + " = " + e.toString() }}
abstract class LTerm extends LExpression
case class LVariable(c: Char) extends LTerm { override def toString() = { c.toString() }}
case class LAbstraction(v: LVariable, m: LTerm) extends LTerm { override def toString() = { "λ" + v + "." + m.toString() }}
case class LApplication(m: LTerm, n: LTerm) extends LTerm { override def toString() = { "(" + m.toString() + " " + n.toString() +")" }}
case class LCapVar(c: Char) extends LTerm { override def toString() = { c.toString() }}


object LambdaParsers extends JavaTokenParsers {
  def expression: Parser[LExpression] = capVarDef | term

  def capVarDef: Parser[LCapVarDef] = """[A-Z]""".r~"="~term ^^ {
    case s~"="~t => LCapVarDef(s.head, t)
  }

  def term: Parser[LTerm] = variable | abstraction | application | capVar

  def variable: Parser[LVariable] = """[a-z]""".r ^^ {s => (LVariable(s.head))}

  def abstraction: Parser[LAbstraction] = "L"~>variable~"."~term ^^ {
    case v~"."~m => LAbstraction(v, m)
  }

  def application: Parser[LApplication] = "("~>term~term<~")" ^^ {
    case m~n => LApplication(m, n)
  }

  def capVar: Parser[LCapVar] = """[A-Z]""".r ^^ {s => LCapVar(s.head)}

  def parseExpr(s: String) = parse(expression, s).get
}



var capVarMap = Map[Char, LTerm]()
def get_cap_var_definition(c: Char) : LTerm = {
  capVarMap get c match {
    case Some(t) => t
    case None => throw new IllegalArgumentException("Variable " + c + " is undefined.")
  }
}

def free_vars(t: LTerm) : Set[LVariable] = t match {
  case LVariable(c) => Set(LVariable(c))
  case LAbstraction(v, m) => free_vars(m) -- Set(v)
  case LApplication(m, n) => free_vars(m) ++ free_vars(n)
  case LCapVar(c) => free_vars(get_cap_var_definition(c))
}


// Fresh vars start at 'n'
// TODO find a better solution to this
var counter = 'n' - 1
def fresh() : LVariable = {
  counter += 1
  LVariable(counter.toChar)
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
  case LCapVar(_) => m
}

// Leftmost-outermost reduction
def reduce(m: LTerm) : LTerm = m match {
  case LApplication(LAbstraction(v, p), n) => substitute(p, v, n)
  case LApplication(LCapVar(c), n) => {
    val cap_var_definition = get_cap_var_definition(c)
    if (free_vars(cap_var_definition).size > 0)
      throw new IllegalArgumentException("Named expressions should not have free vars")
    LApplication(cap_var_definition, n)
  }
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

def is_in_normal_form(m: LTerm) : Boolean = m match {
  case LApplication(LAbstraction(_, _), _) => false
  case LApplication(LCapVar(c), _) => {
    val cap_var_definition = get_cap_var_definition(c)
    if (cap_var_definition.isInstanceOf[LAbstraction])
      false
    else
      is_in_normal_form(cap_var_definition)
  }
  case LVariable(_) => true
  case LAbstraction(_, m) => is_in_normal_form(m)
  case LApplication(m, n) => is_in_normal_form(m) && is_in_normal_form(n)
  case LCapVar(c) => is_in_normal_form(get_cap_var_definition(c))
}

def print_and_normalise(m: LTerm, counter: Int) : Unit = {
  if (counter <= 0) {
    println("...")
  } else {
    println("->β " + m)
    if (!is_in_normal_form(m)) 
      print_and_normalise(reduce(m), counter - 1)
  }
}




println("Ready!")
print("> ")

while (true) {
  val line = scala.io.StdIn.readLine()

  val expr = LambdaParsers.parseExpr(line)

  expr match {
    case LCapVarDef(c, t) => {
      capVarMap = capVarMap ++ Map(c -> t)
      println(expr)
    }
    case _ => {
      val term = expr.asInstanceOf[LTerm]

      println(term.toString())
      if (!is_in_normal_form(term)) 
        print_and_normalise(reduce(term), 100)
    }
  }

  print("> ")
}

