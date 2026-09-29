#include "runtime/standard_primitives.hpp"

#include <chrono>
#include <random>
#include <stdexcept>

namespace goldfish::runtime {
namespace {
using boost::multiprecision::cpp_int;

void
arity (const Values& a, std::size_t n, const char* who) {
  if (a.size () != n) throw std::runtime_error (std::string (who) + " expects " + std::to_string (n) + " arguments");
}
RandomSourceObject&
source (Value v, const char* who) {
  if (!v.is_object () || v.as_object ()->type () != ObjectType::RandomSource)
    throw std::runtime_error (std::string (who) + ": expected random source");
  return *static_cast<RandomSourceObject*> (v.as_object ());
}
cpp_int
exact_integer (Value v, const char* who) {
  if (v.is_integer ()) return cpp_int (v.as_integer ());
  if (is_number (v)) {
    auto n= number_value (v);
    if (n.is_integer () && n.is_exact () && n.is_real ()) return n.real.numerator.native ();
  }
  throw std::runtime_error (std::string (who) + ": expected exact integer");
}
std::uint64_t
word (const cpp_int& n) {
  return static_cast<std::uint64_t> (n & cpp_int (0xffffffffffffffffULL));
}
std::uint64_t
next (RandomSourceObject& s) {
  std::uint64_t x= s.state[0], y= s.state[1];
  s.state[0]= y;
  x^= x << 23;
  s.state[1]= x ^ y ^ (x >> 17) ^ (y >> 26);
  return s.state[1] + y;
}
std::uint64_t
splitmix (std::uint64_t& x) {
  std::uint64_t z= (x+= 0x9e3779b97f4a7c15ULL);
  z              = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
  z              = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
  return z ^ (z >> 31);
}
std::uint64_t
hash_integer (const cpp_int& n, std::uint64_t seed) {
  std::string   text= n.convert_to<std::string> ();
  std::uint64_t h   = 1469598103934665603ULL ^ seed;
  for (unsigned char c : text) {
    h^= c;
    h*= 1099511628211ULL;
  }
  return h;
}
Value
state_value (Evaluator& e, std::uint64_t x) {
  cpp_int b= x;
  return e.number (Number::exact (BigInteger (std::move (b))));
}
} // namespace

void
install_random_primitives (Evaluator& e) {
  e.define_primitive ("g_random-source-create", [&e] (const Values& a) {
    arity (a, 0, "g_random-source-create");
    return Values{Value::object (e.heap ().make<RandomSourceObject> ())};
  });
  e.define_primitive ("g_random-source?", [] (const Values& a) {
    arity (a, 1, "g_random-source?");
    return Values{Value::boolean (a[0].is_object () && a[0].as_object ()->type () == ObjectType::RandomSource)};
  });
  e.define_primitive ("g_random-source-state-ref", [&e] (const Values& a) {
    arity (a, 1, "g_random-source-state-ref");
    auto& s= source (a[0], "random-source-state-ref");
    return Values{
        e.list ({e.symbol ("random-source-state"), state_value (e, s.state[0]), state_value (e, s.state[1])})};
  });
  e.define_primitive ("g_random-source-state-set!", [&e] (const Values& a) {
    arity (a, 2, "g_random-source-state-set!");
    auto&              s= source (a[0], "random-source-state-set!");
    std::vector<Value> xs;
    Value              rest= a[1];
    while (rest.is_object () && rest.as_object ()->type () == ObjectType::Pair) {
      auto* p= static_cast<PairObject*> (rest.as_object ());
      xs.push_back (p->car);
      rest= p->cdr;
    }
    if (!rest.is_null () || xs.size () != 3 || !xs[0].is_object () ||
        xs[0].as_object ()->type () != ObjectType::Symbol ||
        static_cast<SymbolObject*> (xs[0].as_object ())->name != "random-source-state")
      throw std::runtime_error ("random-source-state-set!: invalid state");
    for (std::size_t i= 0; i < 2; ++i) {
      cpp_int v= exact_integer (xs[i + 1], "random-source-state-set!");
      if (v < 0 || v > ((cpp_int (1) << 64) - 1))
        throw std::runtime_error ("random-source-state-set!: invalid state word");
      s.state[i]= word (v);
    }
    if (!(s.state[0] | s.state[1])) throw std::runtime_error ("random-source-state-set!: all-zero state");
    return Values{Value::unspecified ()};
  });
  e.define_primitive ("g_random-source-randomize!", [] (const Values& a) {
    arity (a, 1, "g_random-source-randomize!");
    auto&         s= source (a[0], "random-source-randomize!");
    std::uint64_t seed=
        static_cast<std::uint64_t> (std::chrono::high_resolution_clock::now ().time_since_epoch ().count ());
    try {
      std::random_device rd;
      seed^= (static_cast<std::uint64_t> (rd ()) << 32) ^ rd ();
    } catch (...) {
    }
    for (auto& w : s.state)
      w= splitmix (seed);
    if (!(s.state[0] | s.state[1])) s.state[0]= 1;
    return Values{Value::unspecified ()};
  });
  e.define_primitive ("g_random-source-pseudo-randomize!", [] (const Values& a) {
    arity (a, 3, "g_random-source-pseudo-randomize!");
    auto&   s= source (a[0], "random-source-pseudo-randomize!");
    cpp_int i= exact_integer (a[1], "pseudo-randomize!"), j= exact_integer (a[2], "pseudo-randomize!");
    if (i < 0 || j < 0) throw std::runtime_error ("pseudo-randomize!: expected non-negative indices");
    std::uint64_t       seed = hash_integer (i, 0x6a09e667f3bcc909ULL);
    const std::uint64_t other= hash_integer (j, 0xbb67ae8584caa73bULL);
    seed^= other + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
    for (auto& w : s.state)
      w= splitmix (seed);
    if (!(s.state[0] | s.state[1])) s.state[0]= 1;
    return Values{Value::unspecified ()};
  });
  e.define_primitive ("g_random-source-integer", [&e] (const Values& a) {
    arity (a, 2, "random-integer");
    auto&   s= source (a[0], "random-integer");
    cpp_int n= exact_integer (a[1], "random-integer");
    if (n <= 0) throw std::runtime_error ("random-integer: expected positive exact integer");
    if (n == 1) return Values{e.number (Number::exact (BigInteger (0)))};
    unsigned bits= boost::multiprecision::msb (n - 1) + 1;
    cpp_int  x;
    do {
      x= 0;
      std::uint64_t word= 0;
      unsigned      remaining= 0;
      for (unsigned i= 0; i < bits; ++i) {
        if (remaining == 0) {
          word= next (s);
          remaining= 64;
        }
        x<<= 1;
        x+= word >> 63;
        word<<= 1;
        --remaining;
      }
    } while (x >= n);
    return Values{e.number (Number::exact (BigInteger (std::move (x))))};
  });
  e.define_primitive ("g_random-source-real", [&e] (const Values& a) {
    arity (a, 1, "random-real");
    auto&               s= source (a[0], "random-real");
    const std::uint64_t k= (next (s) >> 11);
    return Values{e.number (Number::inexact ((static_cast<double> (k) + 0.5) / 9007199254740992.0))};
  });
}
} // namespace goldfish::runtime
