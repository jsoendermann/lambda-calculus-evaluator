import scala.util.parsing.combinator._

abstract class LExpression
case class LCapVarDef(c: Char, e: LTerm) extends LExpression
abstract class LTerm extends LExpression
case class LVariable(c: Char) extends LTerm
case class LAbstraction(v: LVariable, m: LTerm) extends LTerm
case class LApplication(m: LTerm, n: LTerm) extends LTerm
case class LCapVar(c: Char) extends LTerm

def expression_to_string(e: LExpression) : String = e match {
  case LCapVarDef(c, e) => c.toString() + " = " + expression_to_string(e)
  case LVariable(c) => c.toString()
  case LAbstraction(v, m) => {
    if (m.isInstanceOf[LAbstraction]) {
      "λ" + expression_to_string(v) + inner_abstraction_to_string(m.asInstanceOf[LAbstraction])
    } else {
      "λ" + expression_to_string(v) + "." + expression_to_string(m)
    }
  }
  // TODO refactor this (extract duplicated code from this case and inner_application_to_string function)
  case LApplication(m, n) => {
    var output = ""

    if (m.isInstanceOf[LApplication]) {
      output += inner_application_to_string(m.asInstanceOf[LApplication])
    } else if (m.isInstanceOf[LAbstraction]) {
      output += "(" + expression_to_string(m) + ")"
    } else {
      output += expression_to_string(m)
    }

    output += " "

    if (n.isInstanceOf[LVariable] || n.isInstanceOf[LCapVar]) {
      output += expression_to_string(n)
    } else {
      output += "(" + expression_to_string(n) + ")"
    }
    
    output
  }
  case LCapVar(c) => c.toString()
}

def inner_abstraction_to_string(a: LAbstraction) : String = {
  if (a.m.isInstanceOf[LAbstraction]) {
    expression_to_string(a.v) + inner_abstraction_to_string(a.m.asInstanceOf[LAbstraction])
  } else {
    expression_to_string(a.v) + "." + expression_to_string(a.m)
  }
}

def inner_application_to_string(a: LApplication) : String = {
  var output = ""

  if (a.m.isInstanceOf[LApplication]) {
    output += inner_application_to_string(a.m.asInstanceOf[LApplication])
  } else if (a.m.isInstanceOf[LAbstraction]) {
    output += "(" + expression_to_string(a.m) + ")"
  } else {
    output += expression_to_string(a.m)
  }

  output += " "

  if (a.n.isInstanceOf[LVariable] || a.n.isInstanceOf[LCapVar]) {
    output += expression_to_string(a.n)
  } else {
    output += "(" + expression_to_string(a.n) + ")"
  }

  output
}


object LambdaParsers extends JavaTokenParsers {
  def expression: Parser[LExpression] = capVarDef | term

  def capVarDef: Parser[LCapVarDef] = """[A-Z]""".r~"="~term ^^ {
    case s~"="~t => LCapVarDef(s.head, t)
  }

  def term: Parser[LTerm] = variable | abstraction | application | capVar

  def variable: Parser[LVariable] = """[a-z]""".r ^^ {s => (LVariable(s.head))}

  def abstraction: Parser[LAbstraction] = "L"~>rep1(variable)~"."~term ^^ {
    case vs~"."~m => {
      vs.init.foldRight(LAbstraction(vs.last, m)){case (v, b) => LAbstraction(v, b)}
    }
  }

  def application: Parser[LApplication] = "("~>term~rep1(term)<~")" ^^ {
    case m~ns => ns.tail.foldLeft(LApplication(m, ns(0))){case (l, r) => LApplication(l, r)}
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
    println("->β " + expression_to_string(m))
    if (!is_in_normal_form(m)) 
      print_and_normalise(reduce(m), counter - 1)
  }
}




println("Ready!")
print("> ")

while (true) {
  val line = scala.io.StdIn.readLine()

  if (line == null) {
    println()
    System.exit(0)
  }

  val expr = LambdaParsers.parseExpr(line)

  expr match {
    case LCapVarDef(c, t) => {
      capVarMap = capVarMap ++ Map(c -> t)
      println(expression_to_string(expr))
    }
    case _ => {
      val term = expr.asInstanceOf[LTerm]

      println(expression_to_string(term))
      if (!is_in_normal_form(term)) 
        print_and_normalise(reduce(term), 100)
    }
  }

  println()
  print("> ")
}

