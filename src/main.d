import std;
import lexer;
import parser;

auto main(string[] args) {
  auto f = stdin;
  if(args.length > 1) f = File(args[1]);

  auto src = cast(string)(f.byChunk(4096).join).idup;
  auto toks = src.lex;
  auto prog = Prog.parse(toks);
}
