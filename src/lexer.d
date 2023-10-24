module lexer;

import std;
import utils;

struct Tok {
  enum Typ { Int, Sign, Rsvd, Name }

  Typ typ;
  size_t line, col;
  string v;
}

enum sign = [
  "<>", "<=", ">=", "<", ">",
   "+", "-", "*", "/", "(", ")", "=", ",", ".", ";", ":=",
];
enum rsvd = [
  "begin", "end", "if", "then", "while", "do", "return", "function", "var", "const",
  "odd", "write", "writeln",
];

// utils

bool isSpc(char c) => c == ' ' || c == '\t' || c == '\r' || c == '\n';
bool isNum(char c) => '0' <= c && c <= '9';
bool isUpper(char c) => 'A' <= c && c <= 'Z';
bool isLower(char c) => 'a' <= c && c <= 'z';
bool isAlph(char c) => c.isUpper || c.isLower;
bool isAlphNum(char c) => c.isAlph || c.isNum;

alias lexRes = Nullable!string;

lexRes just(ref string s, size_t i) {
  auto res = s[0..i].nullable;
  s = s[i..$];
  return res;
}

// spaces (\s*)

lexRes spaces(T: string)(auto ref T s) {
  foreach(i, c; s) if(!c.isSpc)
    return s.just(i);
  return s.just(s.length);
}

unittest {
  static assert("  a  ".spaces.get == "  ");
  static assert(" \t\n ".spaces.get == " \t\n ");
}

// integer (\n+)

lexRes integer(T: string)(auto ref T s) {
  if(!s[0].isNum) return lexRes(); // many1
  foreach(i, c; s) if(!c.isNum)
    return s.just(i);
  return s.just(s.length);
}

unittest {
  static assert("  1234  ".integer.get == "1234");
  static assert("  123!  ".integer.get == "123");
  static assert("  asdf  ".integer.isNull);
}

// name ([a-zA-Z][a-zA-Z\n]*)

lexRes name(T: string)(auto ref T s) {
  if(!s[0].isAlph) return lexRes(); // many1
  foreach(i, c; s) if(!c.isAlphNum)
    return s.just(i);
  return s.just(s.length);
}

unittest {
  static assert("  ab1!  ".name.get == "ab1");
  static assert("  1234  ".name.isNull);
}

// keyword

lexRes keyword(T: string)(auto ref T s, string x) {
  if(s.startsWith(x)) return s.just(x.length);
  return lexRes();
}

unittest {
  static assert("  <><  ".keyword("<>").get == "<>");
  static assert("  <><  ".keyword("!").isNull);
}

// lex

Tok[] lex(string src) {
  Tok[] res;

  foreach(line, s; src.split('\n')) {
    size_t col;

    lex: while(s.length) {
      s.spaces.apply!((s) => col += s.length);

      auto maybeInt = s.integer;
      if(!maybeInt.isNull) {
        auto v = maybeInt.get;
        res ~= Tok(Tok.Typ.Int, line, col, v);
        col += v.length;
        continue;
      }

      static foreach(i; sign) {{
        auto maybeSign = s.keyword(i);
        if(!maybeSign.isNull) {
          auto v = maybeSign.get;
          res ~= Tok(Tok.Typ.Sign, line, col, v);
          col += v.length;
          continue lex;
        }
      }}

      static foreach(i; rsvd) {{
        auto maybeRsvd = s.keyword(i);
        if(!maybeRsvd.isNull) {
          auto v = maybeRsvd.get;
          res ~= Tok(Tok.Typ.Rsvd, line, col, v);
          col += v.length;
          continue lex;
        }
      }}

      auto maybeName = s.name;
      if(!maybeName.isNull) {
        auto v = maybeName.get;
        res ~= Tok(Tok.Typ.Name, line, col, v);
        col += v.length;
        continue;
      }

      fatal("unknown token: ", s);
    }
  }

  return res;
}
