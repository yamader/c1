module lexer;

import std;

struct Tok {
  enum Typ { Int, Sign, Rsvd, Name }

  Typ typ;
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

void spaces(ref string s) {
  foreach(i, c; s) if(!c.isSpc) {
    s = s[i..$];
    return;
  }
  s = "";
}

unittest {
  string s = "  a  ";
  s.spaces;
  assert(s == "a  ");

  s = "    ";
  s.spaces;
  assert(s == "");
}

// integer (\s*\n+)

lexRes integer(T: string)(auto ref T s) {
  s.spaces;
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

// name (\s*[a-zA-Z][a-zA-Z\n]*)

lexRes name(T: string)(auto ref T s) {
  s.spaces;
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
  s.spaces;
  if(s.startsWith(x)) return s.just(x.length);
  return lexRes();
}

unittest {
  static assert("  <><  ".keyword("<>").get == "<>");
  static assert("  <><  ".keyword("!").isNull);
}

// lex

Tok[] lex(string s) {
  Tok[] toks;

  lex: while(s.length) {
    auto maybeInt = s.integer;
    if(!maybeInt.isNull) {
      toks ~= Tok(Tok.Typ.Int, maybeInt.get);
      continue;
    }

    static foreach(i; sign) {{
      auto maybeSign = s.keyword(i);
      if(!maybeSign.isNull) {
        toks ~= Tok(Tok.Typ.Sign, maybeSign.get);
        continue lex;
      }
    }}

    static foreach(i; rsvd) {{
      auto maybeRsvd = s.keyword(i);
      if(!maybeRsvd.isNull) {
        toks ~= Tok(Tok.Typ.Rsvd, maybeRsvd.get);
        continue lex;
      }
    }}

    auto maybeName = s.name;
    if(!maybeName.isNull) {
      toks ~= Tok(Tok.Typ.Name, maybeName.get);
      continue;
    }

    fatal("wtf");
  }

  return toks;
}
