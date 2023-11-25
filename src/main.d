import std;
import lexer;
import parser;

auto main(string[] args) {
  test;

  auto f = stdin;
  if(args.length > 1) f = File(args[1]);

  auto src = cast(string)(f.byChunk(4096).join).idup;
  auto toks = src.lex;
  auto prog = Prog.parse(toks);
  prog.writeln;
}

auto test() {
  writeln("================================================= test =========");
  scope(exit) writeln("============================================= end test =========");

  auto state = "A := B * C / ( D + E ) - F * G";
  auto rp = StateAssign.parse(state.lex).get.rp;
  auto mid = rp.three;

  writeln("statement  : ", state);
  writeln("rev polish : ", rp.join(' '));
  writeln("asm code   : ", mid.map!`a.join(' ')`);
}

auto pop(T)(ref T a) { auto back = a.back; a.popBack; return back; }

auto three(string[] rp) {
  string[][] res;
  string[] stack;
  size_t i;
  while(!rp.empty) {
    auto head = rp.front; rp.popFront;
    if(lexer.sign.canFind(head)) {
      if(head == "=") {
        auto a = stack.pop,
             b = stack.pop;
        res ~= [head, a, b];
      } else {
        auto b = stack.pop,
             a = stack.pop,
             y = 'w' ~ i++.to!string;
        res ~= [head, a, b, y];
        stack ~= y;
      }
    } else stack ~= head;
  }
  return res;
}
