module parser;

import std;
import lexer;
import utils;

// utils

alias tokRes = Maybe!Tok;
alias parseRes = Maybe!(States[]);

alias intv = getHead!(Tok.Typ.Int);
alias name = getHead!(Tok.Typ.Name);
alias rsvd = getValHead!(Tok.Typ.Rsvd);
alias sign = getValHead!(Tok.Typ.Sign);

tokRes getHead(Tok.Typ typ)(ref Tok[] toks) {
  if(toks.length && toks.front.typ == typ) {
    auto head = toks.front;
    toks.popFront;
    return tokRes(head);
  }
  return tokRes();
}

tokRes getValHead(Tok.Typ typ)(ref Tok[] toks, string v) {
  if(toks.length && toks.front.typ == typ && toks.front.v == v) {
    auto head = toks.front;
    toks.popFront;
    return tokRes(head);
  };
  return tokRes();
}

noreturn syntaxErr(Tok tok, string msg) {
  stderr.writeln("syntax error (", msg, ") @ line ", tok.line + 1, ", col ", tok.col + 1);
  throw new Exception("syntax error");
}

// classes

class States {}

class VarDecl: States {
  string name;

  this(string name) {
    this.name = name;
  }

  override string toString() => "var#" ~ name;
}

// parser

parseRes varDecl(T: Tok[])(auto ref T toks) {
  auto maybeHead = toks.rsvd("var");
  if(maybeHead.isNull) return parseRes();

  States[] res;
  while(toks.length) {
    auto x = toks.name;
    if(!x.isNull) res ~= new VarDecl(x.get.v);
    else toks.front.syntaxErr("symbol name expected");

    auto camma = toks.sign(",");
    if(!camma.isNull) continue;

    auto semi = toks.sign(";");
    if(!semi.isNull) return parseRes(res);
    toks.name.apply!((tok) => tok.syntaxErr("comma expected"));
    toks.front.syntaxErr("semicolon expected");
  }

  maybeHead.get.syntaxErr("declaring variable(s)");
}

parseRes prog(T: Tok[])(auto ref T toks) {
  States[] prog;
  while(toks.length) {
    auto res = toks.varDecl | toks.varDecl;
    if(!res.isNull) {
      prog ~= res.get;
      continue;
    }

    // vardecl parse failed; skipping
    toks.popFront;
  }

  return parseRes(prog);
}
