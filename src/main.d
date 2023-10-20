import std;
import lexer;

auto main(string[] args) {
  auto f = stdin;
  if(args.length > 1) f = File(args[1]);
  
  auto tok = f.byLine.map!(a => a.idup.lex).join;
  tok.writeln;
}
