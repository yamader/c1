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

noreturn syntaxErr(Tok tok, string msg) {
  stderr.writeln("syntax error (", msg, ") @ line ", tok.line + 1, ", col ", tok.col + 1);
  throw new Exception("syntax error");
}
noreturn syntaxErr(Tok[] toks, string msg) => toks.front.syntaxErr(msg);

// syntaxes

alias Syntax = Sum!(Block, Decl.Types, State.Types, Cond.Types, Expr, Term, Factor.Types);
alias Syntaxes = Sum!(Syntax.Types, staticMap!(DynArray, Syntax.Types));

struct Prog {
  Block block;

  string toString() => "prog :: " ~ block.toString;

  static Maybe!Prog parse(T: Tok[])(auto ref T toks) {
    auto block = Block.parse(toks);
    if(block.isNull) toks.syntaxErr("block expected");
    if(toks.sign(".")) toks.syntaxErr("dot expected");
    if(!toks.eof) toks.syntaxErr("eof expected");
    return typeof(return)(Prog(block));
  }
}

struct Block {
  Decl[] env;
  State state;

  string toString() => "block#\n" ~ env.map!`"  " ~ a.toString`.join(";\n") ~ state.toString;

  static Maybe!Block parse(T: Tok[])(auto ref T toks) {
    Decl[] env;
    while(toks.length) {
      auto res = ConstDecl.parse(toks) | VarDecl.parse(toks) | FuncDecl.parse(toks);
      if(res.isNull) break;
      env ~= res.get;
    }
    auto state = State.parse(toks);
    if(state.isNull) toks.syntaxErr("statement expected");
    return typeof(return)(Block(env, state.get));
  }
}

alias Decl = Sum!(ConstDecl, VarDecl, FuncDecl);

struct ConstDecl {
  string name, val;

  string toString() => "const#" ~ name ~ ":" ~ val;

  static Maybe!(ConstDecl[]) parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("const").isNull) return typeof(return)();
    ConstDecl[] res;
    while(toks.length) {
      auto x = toks.name;
      if(x.isNull) toks.syntaxErr("symbol name expected");
      if(toks.sign("=").isNull) toks.syntaxErr("equal expected");
      auto v = toks.intv;
      if(v.isNull) toks.syntaxErr("integer value expected");
      res ~= ConstDecl(x.get.v, v.get.v);
      if(!toks.sign(",").isNull) continue;
      if(!toks.sign(";").isNull) return typeof(return)(res);
      toks.name.apply!((tok) => tok.syntaxErr("comma expected"));
      toks.syntaxErr("semicolon expected");
    }
    toks.syntaxErr("declaring constant(s)");
  }
}

struct VarDecl {
  string name;

  string toString() => "var#" ~ name;

  static Maybe!(VarDecl[]) parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("var").isNull) return typeof(return)();
    VarDecl[] res;
    while(toks.eof) {
      auto x = toks.name;
      if(x.isNull) toks.syntaxErr("symbol name expected");
      res ~= VarDecl(x.get.v);
      if(!toks.sign(",").isNull) continue;
      if(!toks.sign(";").isNull) return typeof(return)(res);
      toks.name.apply!((tok) => tok.syntaxErr("comma expected"));
      toks.syntaxErr("semicolon expected");
    }
    toks.syntaxErr("declaring variable(s)");
  }
}

struct FuncDecl {
  string name;
  string[] args;
  Block body;

  string toString() => "func#" ~ name ~ "(" ~ args.join(", ") ~ ")"
    ~ " => { " ~ body.map!`a.toString`.join("; ") ~ " }";

  static Maybe!FuncDecl parse(T: Tok[])(auto ref T toks) {
    if(toks.rsvd("function").isNull) return typeof(return)();
    auto name = toks.name;
    if(name.isNull) toks.syntaxErr("function's name expected");
    string[] args;
    if(toks.sign("(").isNull) toks.syntaxErr("left paren expected for args");
    if(toks.sign(")").isNull) {
      while(toks.length) {
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
    return typeof(return)(FuncDecl(name.get.v, args, block.get));
  }
}

alias State = Sum!(StateAssign);

struct StateAssign {
  string name;
  Expr val;

  string toString() => "assign#" ~ name ~ ":" ~ val.toString;

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

alias Cond = Sum!(CondOdd, CondOpBin);

struct CondOdd {
  Expr expr;

  string toString() => "odd?" ~ expr.toString;

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

  string toString() => lhs.toString ~ op ~ rhs.toString;

  static Maybe!CondOpBin parse(T: Tok[])(auto ref toks) {
    auto lhs = Expr.parse(toks);
  }
}

struct Expr {
  Maybe!string op;
  Term head;
  OpTerm[] tail;

  string toString() => tail.length
    ? head.toString
    : "$"
      ~ (op.isNull ? "" : op.get)
      ~ head.toString
      ~ tail.map!`a.op ~ a.term.toString`.join
      ~ "$";

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
    while(toks.length) {
      auto op = toks.sign("+") | toks.sign("-");
      if(op.isNull) break;
      auto term = Term.parse(toks);
      if(term.isNull) _toks.syntaxErr("term expected");
      tail ~= OpTerm(op.get.v, term.get);
    }
    return typeof(return)(Expr(prefix, head.get, tail));
  }
}

struct Term {
  Factor head;
  OpFactor[] tail;

  string toString() => tail.length
    ? head.toString
    : "("
      ~ head.toString
      ~ tail.map!`a.op ~ a.factor.toString`.join
      ~ ")";

  static Maybe!Term parse(T: Tok[])(auto ref T toks) {
    auto head = Factor.parse(toks);
  }
}
alias OpTerm = Tuple!(string, "op", Term, "term");

struct Factor {
  Sum!(FactorName, FactorNum, FactorCall, FactorParen) v;
  alias v this;
}
alias OpFactor = Tuple!(string, "op", Factor, "factor");

struct FactorName {
  string name;
  string toString() => name;
}

struct FactorNum {
  int val;
  string toString() => val.to!string;
}

struct FactorCall {
  string name;
  Expr[] args;
  string toString() => name ~ "(" ~ args.map!`a.toString`.join(", ") ~ ")";
}

struct FactorParen {
  Expr[] _expr;
  auto expr() => _expr.front;
  this(Expr expr) { this.expr = [expr]; }
  string toString() => "(" ~ expr.front.toString ~ ")";
}
