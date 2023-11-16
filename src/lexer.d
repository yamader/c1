module lexer;

import std;
import utils;

struct Tok {
  enum Typ { Int, Sign, Rsvd, Name, EOF }

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

bool isSpc(C)(C c) => c == ' ' || c == '\t' || c == '\r' || c == '\n';
bool isNum(C)(C c) => '0' <= c && c <= '9';
bool isUpper(C)(C c) => 'A' <= c && c <= 'Z';
bool isLower(C)(C c) => 'a' <= c && c <= 'z';
bool isAlph(C)(C c) => c.isUpper || c.isLower;
bool isAlphNum(C)(C c) => c.isAlph || c.isNum;

alias lexRes = Nullable!string;

lexRes just(ref string s, size_t i) {
  auto res = s[0..i].nullable;
  s = s[i..$];
  return res;
}

// tokens

lexRes spaces(T: string)(auto ref T s) {
  foreach(i, c; s) if(!c.isSpc)
    return s.just(i);
  return s.just(s.length);
}

lexRes integer(T: string)(auto ref T s) {
  if(!s.length || !s.front.isNum) return lexRes(); // many1
  foreach(i, c; s) if(!c.isNum)
    return s.just(i);
  return s.just(s.length);
}

lexRes name(T: string)(auto ref T s) {
  if(!s.length || !s.front.isAlph) return lexRes(); // many1
  foreach(i, c; s) if(!c.isAlphNum)
    return s.just(i);
  return s.just(s.length);
}

lexRes keyword(T: string)(auto ref T s, string x) {
  if(s.startsWith(x)) return s.just(x.length);
  return lexRes();
}

// lex

Tok[] lex(string src) {
  Tok[] res;
  auto eof = Tok(Tok.Typ.EOF);

  foreach(line, s; src.split('\n')) {
    size_t col;

    lex: while(s.length) {
      s.spaces.apply!((s) => col += s.length);
      if(!s.length) break;

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

    eof.line = line;
    eof.col = col;
  }

  return res ~ eof;
}
