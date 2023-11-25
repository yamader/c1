module parser;

import std;
import lexer;
import utils;

// utils

alias intv = getHead!(Tok.Typ.Int);
alias name = getHead!(Tok.Typ.Name);
alias rsvd = getValHead!(Tok.Typ.Rsvd);
alias sign = getValHead!(Tok.Typ.Sign);

Maybe!Tok getHead(Tok.Typ typ)(ref Tok[] toks) {
  if(toks.length && toks.front.typ == typ) {
    auto head = toks.front;
    toks.popFront;
    return typeof(return)(head);
  }
  return typeof(return)();
}

Maybe!Tok getValHead(Tok.Typ typ)(ref Tok[] toks, string v) {
  if(toks.length && toks.front.typ == typ && toks.front.v == v) {
    auto head = toks.front;
    toks.popFront;
    return typeof(return)(head);
  }
  return typeof(return)();
}

bool eof(Tok[] toks) => !toks.length || toks.front.typ == Tok.Typ.EOF;

noreturn syntaxErr(string f = __FILE__, int l = __LINE__)(Tok tok, string msg) {
  stderr.writeln("syntax error (", msg, ") @ line ", tok.line + 1, ", col ", tok.col + 1);
  throw new Exception("syntax error", f, l);
}
noreturn syntaxErr(string f = __FILE__, int l = __LINE__)(Tok[] toks, string msg) =>
  toks.front.syntaxErr!(f, l)(msg);

// syntaxes

struct Prog {
  Block block;

  string toString() => "#prog " ~ block.toString;

  static Prog parse(T: Tok[])(auto ref T toks) {
    auto block = Block.parse(toks);
    if(block.isNull) toks.syntaxErr("block expected");
    if(toks.sign(".").isNull) toks.syntaxErr("dot expected");
    if(!toks.eof) toks.syntaxErr("eof expected");
    return Prog(block.get);
  }
}

class Block {
  Decls[] env;
  States state;

  this(Decls[] env, States state) {
    this.env = env, this.state = state;
  }

  override string toString() =>
    (env.empty ? "" :
      '\n' ~ env.map!(decl => "- " ~ decl.toString.indent).join("\n") ~ '\n')
    ~ state.toString;

  static Maybe!Block parse(T: Tok[])(auto ref T toks) {
    Decls[] env;
    while(!toks.eof) {
      auto res = Decls_parse(toks);
      if(res.isNull) break;
      env ~= res.get.match!(a => a.map!Decls.array);
    }
    auto state = States_parse(toks);
    if(state.isNull) toks.syntaxErr("statement expected");
    return typeof(return)(new Block(env, state.get));
  }
}

alias Decls = Sum!(ConstDecl, VarDecl, FuncDecl);

auto Decls_parse(T: Tok[])(auto ref T toks) =>
  ConstDecl.parse(toks) | VarDecl.parse(toks) | FuncDecl.parse(toks);

struct ConstDecl {
  string name, val;

  string toString() => "const " ~ name ~ " = " ~ val ~ ";";

  static Maybe!(ConstDecl[]) parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("const").isNull) return typeof(return)();
    ConstDecl[] res;
    while(!toks.eof) {
      auto x = toks.name;
      if(x.isNull) toks.syntaxErr("symbol name expected");
      if(toks.sign("=").isNull) toks.syntaxErr("equal expected");
      auto v = toks.intv;
      if(v.isNull) toks.syntaxErr("integer value expected");
      res ~= ConstDecl(x.get.v, v.get.v);
      if(!toks.sign(",").isNull) continue;
      if(!toks.sign(";").isNull) return typeof(return)(res);
      toks.name.run!((tok) => tok.syntaxErr("comma expected"));
      toks.syntaxErr("semicolon expected");
    }
    toks.syntaxErr("declaring constant(s)");
  }
}

struct VarDecl {
  string name;

  string toString() => "var " ~ name ~ ";";

  static Maybe!(VarDecl[]) parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("var").isNull) return typeof(return)();
    VarDecl[] res;
    while(!toks.eof) {
      auto x = toks.name;
      if(x.isNull) toks.syntaxErr("symbol name expected");
      res ~= VarDecl(x.get.v);
      if(!toks.sign(",").isNull) continue;
      if(!toks.sign(";").isNull) return typeof(return)(res);
      toks.name.run!((tok) => tok.syntaxErr("comma expected"));
      toks.syntaxErr("semicolon expected");
    }
    toks.syntaxErr("declaring variable(s)");
  }
}

struct FuncDecl {
  string name;
  string[] args;
  Block body;

  string toString() =>
    "fn " ~ name ~ "(" ~ args.join(", ") ~ ") " ~ body.toString;

  static Maybe!(FuncDecl[]) parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("function").isNull) return typeof(return)();
    auto name = toks.name;
    if(name.isNull) toks.syntaxErr("function's name expected");
    string[] args;
    if(toks.sign("(").isNull) toks.syntaxErr("left paren expected for args");
    if(toks.sign(")").isNull) {
      while(!toks.eof) {
        auto arg = toks.name;
        if(arg.isNull) toks.syntaxErr("argument name expected");
        args ~= arg.get.v;
        if(toks.sign(",").isNull) break;
      }
      if(toks.sign(")").isNull) toks.syntaxErr("right paren expected for args");
    }
    auto block = Block.parse(toks);
    if(block.isNull) toks.syntaxErr("block expected for function body");
    if(toks.sign(";").isNull) toks.syntaxErr("function must be end with semicolon");
    return typeof(return)([FuncDecl(name.get.v, args, block.get)]);
  }
}

alias States = Sum!(StateAssign, StateBlock, StateIf, StateWhile, StateReturn,
                    StateWrite, StateWriteln);

Maybe!States States_parse(T: Tok[])(auto ref T toks)
  => StateAssign.parse(toks)
   | StateBlock.parse(toks)
   | StateIf.parse(toks)
   | StateWhile.parse(toks)
   | StateReturn.parse(toks)
   | StateWrite.parse(toks)
   | StateWriteln.parse(toks);

struct StateAssign {
  string name;
  Expr val;

  string toString() => name ~ " := " ~ val.toString;

  string[] rp() => name ~ val.rp ~ "=";

  static Maybe!StateAssign parse(T: Tok[])(auto ref T toks) {
    auto _toks = toks.dup;
    auto x = _toks.name;
    if(x.isNull) return typeof(return)();
    if(_toks.sign(":=").isNull) return typeof(return)();
    toks = _toks;
    auto v = Expr.parse(toks);
    if(v.isNull) toks.syntaxErr("expression expected");
    return typeof(return)(StateAssign(x.get.v, v.get));
  }
}

struct StateBlock {
  States[] states;

  string toString() =>
    "begin\n" ~ states.map!(s => "  " ~ s.toString.indent).join(";\n") ~ ";\nend";

  static Maybe!StateBlock parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("begin").isNull) return typeof(return)();
    auto head = States_parse(toks);
    if(head.isNull) toks.syntaxErr("statement expected");
    States[] states;
    while(!toks.eof) {
      if(toks.sign(";").isNull) break;
      auto state = States_parse(toks);
      if(state.isNull) break; // ??
      states ~= state.get;
    }
    if(toks.rsvd("end").isNull) toks.syntaxErr("end expected");
    return typeof(return)(StateBlock(states));
  }
}

class StateIf {
  Conds cond;
  States state;

  this(Conds cond, States state) {
    this.cond = cond, this.state = state;
  }

  override string toString() => "if"
    ~ "(" ~ cond.toString ~ ") " ~ state.toString;

  static Maybe!StateIf parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("if").isNull) return typeof(return)();
    auto cond = Conds_parse(toks);
    if(cond.isNull) toks.syntaxErr("condition expected");
    if(toks.rsvd("then").isNull) toks.syntaxErr("then expected");
    auto state = States_parse(toks);
    if(state.isNull) toks.syntaxErr("statement expected");
    return typeof(return)(new StateIf(cond.get, state.get));
  }
}

class StateWhile {
  Conds cond;
  States state;

  this(Conds cond, States state) {
    this.cond = cond, this.state = state;
  }

  override string toString() => "while"
    ~ "(" ~ cond.toString ~ ") " ~ state.toString;

  static Maybe!StateWhile parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("while").isNull) return typeof(return)();
    auto cond = Conds_parse(toks);
    if(cond.isNull) toks.syntaxErr("condition expected");
    if(toks.rsvd("do").isNull) toks.syntaxErr("do expected");
    auto state = States_parse(toks);
    if(state.isNull) toks.syntaxErr("statement expected");
    return typeof(return)(new StateWhile(cond.get, state.get));
  }
}

struct StateReturn {
  Expr expr;

  string toString() => "return " ~ expr.toString;

  static Maybe!StateReturn parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("return").isNull) return typeof(return)();
    auto expr = Expr.parse(toks);
    if(expr.isNull) toks.syntaxErr("expression expected");
    return typeof(return)(StateReturn(expr.get));
  }
}

struct StateWrite {
  Expr expr;

  string toString() => "write " ~ expr.toString;

  static Maybe!StateWrite parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("write").isNull) return typeof(return)();
    auto expr = Expr.parse(toks);
    if(expr.isNull) toks.syntaxErr("expression expected");
    return typeof(return)(StateWrite(expr.get));
  }
}

struct StateWriteln {
  string toString() => "writeln";

  static Maybe!StateWriteln parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("writeln").isNull) return typeof(return)();
    return typeof(return)(StateWriteln());
  }
}

alias Conds = Sum!(CondOdd, CondOpBin);

auto Conds_parse(T: Tok[])(auto ref T toks) =>
  CondOdd.parse(toks) | CondOpBin.parse(toks);

struct CondOdd {
  Expr expr;

  string toString() => "odd? " ~ expr.toString;

  static Maybe!CondOdd parse(T: Tok[])(auto ref T toks) {
    auto odd = toks.rsvd("odd");
    if(odd.isNull) return typeof(return)();
    auto expr = Expr.parse(toks);
    if(expr.isNull) toks.syntaxErr("expression expected");
    return typeof(return)(CondOdd(expr.get));
  }
}

struct CondOpBin {
  Expr lhs, rhs;
  string op;

  string toString() => [lhs.toString, op, rhs.toString].join(' ');

  static Maybe!CondOpBin parse(T: Tok[])(auto ref T toks) {
    auto _toks = toks.dup;
    auto lhs = Expr.parse(_toks);
    if(lhs.isNull) return typeof(return)();
    toks = _toks;
    auto op = toks.sign("=")
            | toks.sign("<>")
            | toks.sign("<=")
            | toks.sign(">=")
            | toks.sign("<")
            | toks.sign(">");
    if(op.isNull) toks.syntaxErr("condition operator expected");
    auto rhs = Expr.parse(toks);
    if(rhs.isNull) toks.syntaxErr("rhs expression expected");
    return typeof(return)(CondOpBin(lhs.get, rhs.get, op.get.v));
  }
}

class Expr {
  Maybe!string op;
  Term head;
  OpTerm[] tail;

  this(Maybe!string op, Term head, OpTerm[] tail) {
    this.op = op, this.head = head, this.tail = tail;
  }

  override string toString() => (op.isNull ? "" : op.get) ~ head.toString
    ~ (tail.empty ? "" : tail.map!`a.op ~ a.term.toString`.join);

  string[] rp() {
    string[] res = head.rp;
    foreach(op, term; tail.assocArray)
      res ~= term.rp ~ op;
    return res;
  }

  static Maybe!Expr parse(T: Tok[])(auto ref T toks) {
    auto op = Maybe!string();
    auto _op = toks.sign("+") | toks.sign("-");
    if(!_op.isNull) op = _op.get.v;
    auto head = Term.parse(toks);
    if(head.isNull) {
      if(!op.isNull) toks.syntaxErr("term expected");
      return typeof(return)();
    }
    OpTerm[] tail;
    while(!toks.eof) {
      auto prefix = toks.sign("+") | toks.sign("-");
      if(prefix.isNull) break;
      auto term = Term.parse(toks);
      if(term.isNull) toks.syntaxErr("term expected");
      tail ~= OpTerm(prefix.get.v, term.get);
    }
    return typeof(return)(new Expr(op, head.get, tail));
  }
}

struct Term {
  Factor head;
  OpFactor[] tail;

  string toString() => head.toString ~
    (tail.empty ? "" : tail.map!`a.op ~ a.factor.toString`.join);

  string[] rp() {
    string[] res = head.rp;
    foreach(op, factor; tail.assocArray)
      res ~= factor.rp ~ op;
    return res;
  }

  static Maybe!Term parse(T: Tok[])(auto ref T toks) {
    auto head = Factor_parse(toks);
    if(head.isNull) return typeof(return)();
    OpFactor[] tail;
    while(!toks.eof) {
      auto op = toks.sign("*") | toks.sign("/");
      if(op.isNull) break;
      auto factor = Factor_parse(toks);
      if(factor.isNull) toks.syntaxErr("factor expected");
      tail ~= OpFactor(op.get.v, factor.get);
    }
    return typeof(return)(Term(head.get, tail));
  }
}
alias OpTerm = Tuple!(string, "op", Term, "term");

alias Factor = Sum!(FactorCall, FactorParen, string, int);
alias OpFactor = Tuple!(string, "op", Factor, "factor");

auto rp(Factor f) => f.match!(
    (string s) => [s],
    (int i)    => [i.to!string],
    f          => f.rp);

auto Factor_parse(T: Tok[])(auto ref T toks)
  => FactorCall.parse(toks)
   | FactorParen.parse(toks)
   | toks.name.bind!(m => m.isNull ? Maybe!string() : Maybe!string(m.get.v))
   | toks.intv.bind!(m => m.isNull ? Maybe!int() : Maybe!int(m.get.v.to!int));

struct FactorCall {
  string name;
  Expr[] args;

  string toString() => name ~ " " ~ args.map!`a.toString`.join(", ");

  string[] rp() => ["call " ~ toString];

  static Maybe!FactorCall parse(T: Tok[])(auto ref T toks) {
    auto _toks = toks.dup;
    auto f = _toks.name;
    if(f.isNull) return typeof(return)();
    if(_toks.sign("(").isNull) return typeof(return)();
    toks = _toks;
    Expr[] args;
    while(!toks.eof) {
      if(!toks.sign(")").isNull) break;
      auto expr = Expr.parse(toks);
      if(expr.isNull) toks.syntaxErr("expression expected");
      args ~= expr.get;
      toks.sign(",");
    }
    return typeof(return)(FactorCall(f.get.v, args));
  }
}

struct FactorParen {
  Expr expr;

  string toString() => "(" ~ expr.toString ~ ")";

  string[] rp() => expr.rp;

  static Maybe!FactorParen parse(T: Tok[])(auto ref T toks) {
    if(toks.sign("(").isNull) return typeof(return)();
    auto expr = Expr.parse(toks);
    if(expr.isNull) toks.syntaxErr("expression expected");
    if(toks.sign(")").isNull) toks.syntaxErr("closing paren expected");
    return typeof(return)(FactorParen(expr.get));
  }
}
