module utils;

import std;

alias DynArray(T) = T[];

struct Maybe(T) {
  Nullable!T v;
  alias v this;

  this(T just) { v = typeof(v)(just); }

  auto opBinary(string op: "|", U)(lazy Maybe!U rhs) {
    static if(isSumType!T) alias LHS = TemplateArgsOf!T;
    else                   alias LHS = T;
    static if(isSumType!U) alias RHS = TemplateArgsOf!U;
    else                   alias RHS = U;
    alias A = NoDuplicates!(LHS, RHS), V = Sum!(A);
    static if(A.length == 1) return isNull ? rhs : this;
    else                     return isNull ? rhs.to!V : to!V;
  }

  auto to(U)() => isNull ? Maybe!U() : Maybe!U(U(v.get));
}

struct Sum(T...) {
  SumType!T v;
  alias v this;

  this(U...)(U args) {
    static if(isSumType!U) v = args.match!(typeof(v));
    else                   v = typeof(v)(args);
  }
}

auto apply(alias f, T)(Maybe!T maybe) => maybe.isNull ? maybe : f(maybe.get);
