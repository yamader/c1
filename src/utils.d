module utils;

import std;

struct Maybe(T) {
  Nullable!T v;
  alias this = v;

  this(T just) {
    v = Nullable!T(just);
  }

  auto opBinary(string op: "|")(lazy Maybe!T rhs) => isNull ? rhs : this;

  auto apply(alias f)() => isNull ? this : f(v.get);
}
