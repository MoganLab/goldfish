//
// gf0_eval.cpp -- reference evaluator for lowered core scheme (T0-ahead).
//
// The first permanent brick of the s7 replacement: a tree-walking evaluator
// for `ir->core` output (CORE-SEMANTICS.md), seeing ONLY the gf:: surface
// (this TU must never include s7.h nor call s7_*; lint enforces it).
// s7 stays as reader/object model; gf0 owns env frames + closures.
//
// Scope (M1b): quote if begin lambda define set! let let* letrec letrec*
//   let-values values call-with-values module-ref module-set + calls.
//   define: (define name exp) only; sugar stays frontend.
//   module-ref: generic call into the kernel substrate (rootlet-bound after
//     boot); module-set is (set! (module-ref 'l 'n) v) with write-through
//     via (setter module-ref). The ((setter module-ref) ..) shape needs no
//     special case (generic call path handles it).
//   Multi-values: eval yields V (single|multi); s7 calls go through the
//     same eval-wrap so arity-0..n all collect uniformly (quoting makes
//     the wrap value-exact).
//   Interop: s7 procedures apply via a (call-with-values thunk list)
//     eval-wrap (uniform 0..n collection, s7 collapse rule); gf0 closures
//     crossing into s7 are wrapped in a trampoline calling back through
//     g_gf0-apply (callbacks must be single-valued; closure eq? across
//     the boundary is not preserved).
//   R7RS-strict (intentional s7 divergences, see CORE-SEMANTICS.md):
//     single unspecified delivers 1 value; letrec init-period reads error.
// Known limits (by design, not bugs): no TCO (C++ stack bounds recursion),
//   no unprotect (all held pointers are pinned for the session; debug-scale
//   only), symbols compared by name (interning-robust), toplevel cells still
//   delegate to the s7 rootlet (M2).
//
// Entry: (g_gf0-eval datum) evaluates one lowered datum in the session top
// env (defines persist across datums of one process).
// Driver: `gf eval-gf0 CODE` reads data forms, prints each result as a list
// (call-with-values + list), so multi-values display uniformly.
//

#include "gf.h"

#include <cstring>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

namespace goldfish {
namespace gf0 {

using gf::pointer;
using gf::scheme;

// Eval result: exactly one value, or 0..n values.
struct V {
  bool                 multi;
  pointer              one;  // valid when !multi
  std::vector<pointer> many; // valid when multi
};
static V
single (pointer p) {
  V v;
  v.multi= false;
  v.one  = p;
  return v;
}
static V
multi_vec (std::vector<pointer>&& m) {
  V v;
  v.multi= true;
  v.many = std::move (m);
  return v;
}

// Persistent lexical env: frames[0] is outermost; innermost last. Each
// frame is individually shared so set! through any captured env mutates the
// one frame object every holder sees.
struct Binding {
  pointer sym;
  pointer val;
};
struct Frame {
  std::vector<Binding> bindings;
};
struct Env {
  std::shared_ptr<std::vector<std::shared_ptr<Frame>>> frames;
};
struct Closure {
  pointer formals;
  pointer body; // list of body exprs
  Env     env;
};

static std::vector<Closure>      s_registry;
static std::unordered_set<void*> s_boxes; // c_object boxes holding a registry index
static Env                       s_top;   // session env (defines persist)
static pointer                   s_unassigned= nullptr; // letrec slot sentinel

static pointer
pin (scheme* sc, pointer p) {
  gf::gc_protect (sc, p);
  return p;
}

static pointer
fail (scheme* sc, const char* msg, pointer irritant) {
  return gf::error (sc, gf::make_symbol (sc, "gf0-error"),
                    gf::list (sc, gf::make_string (sc, msg), irritant));
}

static pointer
unassigned_box (scheme* sc) {
  if (s_unassigned == nullptr) {
    s_unassigned= pin (sc, gf::make_c_object_with_let (
                           sc, gf::make_c_type (sc, "gf0-unassigned"),
                           nullptr, gf::nil (sc)));
  }
  return s_unassigned;
}

static bool
is_head (pointer x, const char* name) {
  return gf::is_pair (x) && gf::is_symbol (gf::car (x)) &&
         std::strcmp (gf::symbol_name (gf::car (x)), name) == 0;
}

// R7RS syntax owned by the frontend: valid core never calls these as
// procedures (CORE-SEMANTICS.md). s7 binds some to procedure-like syntax
// objects, so resolve-before-apply would misdispatch. Fail fast -- but only
// when the name is NOT lexically shadowed.
static const char* kFrontendSyntax[] = {
  "cond",   "case",   "and",   "or",   "when",  "unless", "do",
  "delay",  "quasiquote", "unquote",   "unquote-splicing", "define-syntax",
  "syntax-rules", nullptr,
};

static bool
frame_has (pointer sym, const std::shared_ptr<Frame>& f) {
  const char* want= gf::symbol_name (sym);
  for (size_t i= f->bindings.size (); i-- > 0;)
    if (std::strcmp (gf::symbol_name (f->bindings[i].sym), want) == 0)
      return true;
  return false;
}

static bool
lexically_bound (pointer sym, Env env) {
  for (size_t i= env.frames->size (); i-- > 0;)
    if (frame_has (sym, (*env.frames)[i])) return true;
  return false;
}

static bool
is_frontend_syntax (pointer sym, Env env) {
  if (!gf::is_symbol (sym) || lexically_bound (sym, env)) return false;
  const char* want= gf::symbol_name (sym);
  for (const char** k= kFrontendSyntax; *k; ++k)
    if (std::strcmp (want, *k) == 0) return true;
  return false;
}

static V eval (scheme* sc, pointer x, Env env);

static pointer
args_to_list (scheme* sc, const std::vector<pointer>& argv);

static pointer
must_single (scheme* sc, V v, pointer ctx, const char* what) {
  if (v.multi)
    return fail (sc, what, ctx); // fail longjmps; return keeps form
  return v.one;
}

static pointer
lookup_raw (scheme* sc, pointer sym, Env env) {
  const char* want= gf::symbol_name (sym);
  for (size_t i= env.frames->size (); i-- > 0;) {
    const std::vector<Binding>& bs= (*env.frames)[i]->bindings;
    for (size_t j= bs.size (); j-- > 0;)
      if (std::strcmp (gf::symbol_name (bs[j].sym), want) == 0)
        return bs[j].val;
  }
  // Scaffolding: fall back to the s7 rootlet (toplevel cells, M2).
  if (gf::is_defined (sc, want)) return gf::name_to_value (sc, want);
  return fail (sc, "gf0: unbound variable", sym);
}

// s7 call with uniform multi collection: (call-with-values
// (lambda () (PROC 'A ...)) list) always yields a proper list, even for
// 0 results. Quoting is value-exact for every type (symbols included).
// gf0 closures crossing into s7 are wrapped in a trampoline lambda that
// calls back via g_gf0-apply (s7 cannot apply c_object boxes; callbacks
// must be single-valued).
static pointer
wrap_for_s7 (scheme* sc, pointer box) {
  pointer quote_sym= pin (sc, gf::make_symbol (sc, "quote"));
  pointer inner= pin (sc, gf::list (sc,
    pin (sc, gf::make_symbol (sc, "g_gf0-apply")),
    pin (sc, gf::list (sc, quote_sym, box)),
    pin (sc, gf::make_symbol (sc, "args"))));
  pointer expr= pin (sc, gf::list (sc,
    pin (sc, gf::make_symbol (sc, "lambda")),
    pin (sc, gf::make_symbol (sc, "args")),
    inner));
  return pin (sc, gf::eval (sc, expr, gf::rootlet (sc)));
}

static V
s7call_vec (scheme* sc, pointer proc, const std::vector<pointer>& argv) {
  pointer quote_sym= pin (sc, gf::make_symbol (sc, "quote"));
  std::vector<pointer> qargs;
  qargs.reserve (argv.size ());
  for (pointer a : argv) {
    pointer v= (s_boxes.find ((void*) a) != s_boxes.end ()) ? wrap_for_s7 (sc, a) : a;
    qargs.push_back (pin (sc, gf::list (sc, quote_sym, v)));
  }
  pointer callexpr= pin (sc, gf::cons (sc, pin (sc, gf::list (sc, quote_sym, proc)),
                                       pin (sc, args_to_list (sc, qargs))));
  pointer thunk= pin (sc, gf::list (sc, pin (sc, gf::make_symbol (sc, "lambda")),
                                    gf::nil (sc), callexpr));
  pointer expr= pin (sc, gf::list (sc, pin (sc, gf::make_symbol (sc, "call-with-values")),
                                   thunk, pin (sc, gf::make_symbol (sc, "list"))));
  pointer collected= gf::eval (sc, expr, gf::rootlet (sc));
  std::vector<pointer> out;
  pointer tail= collected;
  for (; gf::is_pair (tail); tail= gf::cdr (tail))
    out.push_back (pin (sc, gf::car (tail)));
  if (!gf::is_null (sc, tail))
    return multi_vec (std::vector<pointer>{fail (sc, "gf0: improper collection", collected)});
  // s7 collapse rule: exactly 1 yielded value is single (so (define x (+ 1 2))
  // stays single); 0 or 2+ stay multi. Matches the s7/guile oracle.
  if (out.size () == 1) return single (out[0]);
  return multi_vec (std::move (out));
}

static void
evlis_single (scheme* sc, pointer args, Env env, std::vector<pointer>& out) {
  for (; gf::is_pair (args); args= gf::cdr (args))
    out.push_back (pin (sc, must_single (sc, eval (sc, gf::car (args), env),
                                         gf::car (args), "gf0: arg must be single-valued")));
  if (!gf::is_null (sc, args)) fail (sc, "gf0: improper arg list", args);
}

static pointer
args_to_list (scheme* sc, const std::vector<pointer>& argv) {
  if (argv.empty ()) return gf::nil (sc);
  return gf::array_to_list (sc, (gf::int_) argv.size (),
                            const_cast<pointer*> (argv.data ()));
}

// Bind formals (proper / dotted / single symbol) to a value LIST.
static void
bind_formals (scheme* sc, pointer formals, pointer arglist,
              std::vector<Binding>& frame, pointer ctx) {
  for (; gf::is_pair (formals); formals= gf::cdr (formals)) {
    if (!gf::is_pair (arglist))
      fail (sc, "gf0: too few arguments", ctx);
    pointer name= gf::car (formals);
    if (!gf::is_symbol (name)) fail (sc, "gf0: non-symbol formal", name);
    frame.push_back ({pin (sc, name), pin (sc, gf::car (arglist))});
    arglist= gf::cdr (arglist);
  }
  if (gf::is_symbol (formals)) { // rest arg
    frame.push_back ({pin (sc, formals), pin (sc, arglist)});
  }
  else if (!gf::is_null (sc, formals)) {
    fail (sc, "gf0: improper formals", formals);
  }
  else if (gf::is_pair (arglist)) {
    fail (sc, "gf0: too many arguments", ctx);
  }
}

static Env
push_frame (Env env) {
  Env inner= env;
  inner.frames= std::make_shared<std::vector<std::shared_ptr<Frame>>> (*env.frames);
  inner.frames->push_back (std::make_shared<Frame> ());
  return inner;
}

static V
apply_closure (scheme* sc, const Closure& c, pointer arglist, pointer ctx) {
  Env                   inner= push_frame (c.env);
  std::vector<Binding>& frame= inner.frames->back ()->bindings;
  bind_formals (sc, c.formals, arglist, frame, ctx);
  V r= single (gf::unspecified (sc));
  for (pointer b= c.body; gf::is_pair (b); b= gf::cdr (b))
    r= eval (sc, gf::car (b), inner);
  return r;
}

static V
apply_values (scheme* sc, pointer proc, const std::vector<pointer>& many, pointer ctx) {
  if (s_boxes.find ((void*) proc) != s_boxes.end ()) {
    gf::int_ idx= (gf::int_) (intptr_t) gf::c_object_value (proc);
    if (idx < 0 || (size_t) idx >= s_registry.size ())
      return single (fail (sc, "gf0: stale closure", proc));
    return apply_closure (sc, s_registry[(size_t) idx], args_to_list (sc, many), ctx);
  }
  if (gf::is_procedure (proc)) return s7call_vec (sc, proc, many);
  return single (fail (sc, "gf0: cannot apply (macros stay frontend)", proc));
}

static V
eval (scheme* sc, pointer x, Env env) {
  // Self-evaluating.
  if (gf::is_boolean (x) || gf::is_number (x) || gf::is_string (x) ||
      gf::is_character (x) || gf::is_null (sc, x) || gf::is_vector (x))
    return single (x);
  if (gf::is_symbol (x)) {
    pointer v= lookup_raw (sc, x, env);
    if (v == unassigned_box (sc))
      return single (fail (sc, "gf0: read before assignment (R7RS letrec)", x));
    return single (v);
  }
  if (!gf::is_pair (x)) return single (fail (sc, "gf0: cannot evaluate", x));

  if (is_head (x, "quote")) {
    if (!gf::is_pair (gf::cdr (x)) || gf::is_pair (gf::cdr (gf::cdr (x))))
      return single (fail (sc, "gf0: bad quote", x));
    return single (gf::car (gf::cdr (x)));
  }
  if (is_head (x, "if")) {
    // NOTE: s7 nil is a live pointer, never nullptr; shape checks must use
    // is_pair/is_null, not pointer comparison.
    pointer rest1= gf::cdr (x);
    if (!gf::is_pair (rest1)) return single (fail (sc, "gf0: bad if", x));
    pointer rest2= gf::cdr (rest1);
    if (!gf::is_pair (rest2)) return single (fail (sc, "gf0: bad if", x));
    pointer rest3= gf::cdr (rest2);
    if (!(gf::is_null (sc, rest3) ||
          (gf::is_pair (rest3) && gf::is_null (sc, gf::cdr (rest3)))))
      return single (fail (sc, "gf0: bad if", x));
    pointer t= must_single (sc, eval (sc, gf::car (rest1), env), x,
                            "gf0: if test must be single-valued");
    if (!gf::boolean (sc, t))
      return gf::is_pair (rest3) ? eval (sc, gf::car (rest3), env)
                                 : single (gf::unspecified (sc));
    return eval (sc, gf::car (rest2), env);
  }
  if (is_head (x, "begin")) {
    V r= single (gf::unspecified (sc));
    for (pointer b= gf::cdr (x); gf::is_pair (b); b= gf::cdr (b))
      r= eval (sc, gf::car (b), env);
    return r;
  }
  if (is_head (x, "lambda")) {
    if (!gf::is_pair (gf::cdr (x)))
      return single (fail (sc, "gf0: bad lambda", x));
    Closure c;
    c.formals= pin (sc, gf::car (gf::cdr (x)));
    c.body   = pin (sc, gf::cdr (gf::cdr (x)));
    c.env    = env;
    s_registry.push_back (c);
    gf::int_ idx= (gf::int_) (s_registry.size () - 1);
    pointer  box= pin (sc, gf::make_c_object_with_let (
                         sc, gf::make_c_type (sc, "gf0-closure"),
                         (void*) (intptr_t) idx, gf::nil (sc)));
    s_boxes.insert ((void*) box);
    return single (box);
  }
  if (is_head (x, "define")) {
    pointer rest= gf::cdr (x);
    bool ok= gf::is_pair (rest) && gf::is_symbol (gf::car (rest)) &&
             gf::is_pair (gf::cdr (rest)) &&
             gf::is_null (sc, gf::cdr (gf::cdr (rest)));
    if (!ok)
      return single (fail (sc, "gf0: only (define name exp); sugar stays frontend", x));
    pointer name= gf::car (rest);
    pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (rest)), env), x,
                                     "gf0: define init must be single-valued"));
    env.frames->back ()->bindings.push_back ({pin (sc, name), v});
    return single (gf::unspecified (sc));
  }
  if (is_head (x, "set!")) {
    pointer rest= gf::cdr (x);
    // (set! (module-ref 'l 'n) v): write-through to the runtime module.
    if (gf::is_pair (rest) && gf::is_pair (gf::car (rest)) &&
        is_head (gf::car (rest), "module-ref")) {
      pointer mr= gf::car (rest);
      pointer tail= gf::cdr (rest);
      if (!gf::is_pair (tail) || gf::is_pair (gf::cdr (tail)) ||
          !gf::is_pair (gf::cdr (mr)) || !gf::is_pair (gf::cdr (gf::cdr (mr))) ||
          gf::is_pair (gf::cdr (gf::cdr (gf::cdr (mr)))))
        return single (fail (sc, "gf0: bad module-set", x));
      pointer lib= must_single (sc, eval (sc, gf::car (gf::cdr (mr)), env), x,
                                "gf0: module lib must be single-valued");
      pointer name= must_single (sc, eval (sc, gf::car (gf::cdr (gf::cdr (mr))), env), x,
                                 "gf0: module name must be single-valued");
      pointer v= must_single (sc, eval (sc, gf::car (tail), env), x,
                              "gf0: module-set value must be single-valued");
      pointer mr_proc= lookup_raw (sc, pin (sc, gf::make_symbol (sc, "module-ref")), env);
      pointer setter= lookup_raw (sc, pin (sc, gf::make_symbol (sc, "setter")), env);
      if (mr_proc == unassigned_box (sc) || !gf::is_procedure (mr_proc) ||
          setter == unassigned_box (sc) || !gf::is_procedure (setter))
        return single (fail (sc, "gf0: module-ref unavailable (boot substrate?)", x));
      std::vector<pointer> one;
      one.push_back (mr_proc);
      pointer writer= must_single (sc, s7call_vec (sc, setter, one), x,
                                   "gf0: (setter module-ref) must be single");
      std::vector<pointer> args3;
      args3.push_back (lib);
      args3.push_back (name);
      args3.push_back (v);
      must_single (sc, s7call_vec (sc, writer, args3), x,
                   "gf0: module write must be single-valued");
      return single (gf::unspecified (sc));
    }
    bool ok= gf::is_pair (rest) && gf::is_symbol (gf::car (rest)) &&
             gf::is_pair (gf::cdr (rest)) &&
             gf::is_null (sc, gf::cdr (gf::cdr (rest)));
    if (!ok) return single (fail (sc, "gf0: bad set!", x));
    pointer name= gf::car (rest);
    pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (rest)), env), x,
                                     "gf0: set! value must be single-valued"));
    const char* want= gf::symbol_name (name);
    for (size_t i= env.frames->size (); i-- > 0;) {
      std::vector<Binding>& bs= (*env.frames)[i]->bindings;
      for (size_t j= bs.size (); j-- > 0;) {
        if (std::strcmp (gf::symbol_name (bs[j].sym), want) == 0) {
          bs[j].val= v;
          return single (gf::unspecified (sc));
        }
      }
    }
    if (gf::is_defined (sc, want)) { // s7 toplevel cell (scaffolding, M2)
      gf::define (sc, gf::rootlet (sc), name, v);
      return single (gf::unspecified (sc));
    }
    return single (fail (sc, "gf0: set! of unbound variable", name));
  }
  if (is_head (x, "let")) {
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return single (fail (sc, "gf0: bad let", x));
    Env inner= push_frame (env);
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return single (fail (sc, "gf0: bad let binding", x));
      pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (b)), env), x,
                                       "gf0: let init must be single-valued"));
      inner.frames->back ()->bindings.push_back ({pin (sc, gf::car (b)), v});
    }
    V r= single (gf::unspecified (sc));
    for (pointer b= gf::cdr (rest); gf::is_pair (b); b= gf::cdr (b))
      r= eval (sc, gf::car (b), inner);
    return r;
  }
  if (is_head (x, "let*")) {
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return single (fail (sc, "gf0: bad let*", x));
    Env inner= push_frame (env);
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return single (fail (sc, "gf0: bad let* binding", x));
      pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (b)), inner), x,
                                       "gf0: let* init must be single-valued"));
      inner.frames->back ()->bindings.push_back ({pin (sc, gf::car (b)), v});
    }
    V r= single (gf::unspecified (sc));
    for (pointer b= gf::cdr (rest); gf::is_pair (b); b= gf::cdr (b))
      r= eval (sc, gf::car (b), inner);
    return r;
  }
  if (is_head (x, "letrec") || is_head (x, "letrec*")) {
    bool ordered= (std::strcmp (gf::symbol_name (gf::car (x)), "letrec*") == 0);
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return single (fail (sc, "gf0: bad letrec", x));
    Env inner= push_frame (env);
    std::vector<Binding>& frame= inner.frames->back ()->bindings;
    // Validate + pre-allocate all slots unassigned (R7RS: init-period reads
    // of any slot are an error; the reader reports the slot, not #<undefined>).
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return single (fail (sc, "gf0: bad letrec binding", x));
      frame.push_back ({pin (sc, gf::car (b)), unassigned_box (sc)});
    }
    size_t k= 0;
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs), ++k) {
      // Sequential assignment refines R7RS's unspecified init order; valid
      // programs (no init-period refs) behave identically either way.
      pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (gf::car (bs))), inner),
                                       x, "gf0: letrec init must be single-valued"));
      frame[k].val= v;
    }
    V r= single (gf::unspecified (sc));
    for (pointer b= gf::cdr (rest); gf::is_pair (b); b= gf::cdr (b))
      r= eval (sc, gf::car (b), inner);
    return r;
  }
  if (is_head (x, "let-values")) {
    // Lowered code already has call-with-values; accept raw let-values by
    // evaluating each init in the OUTER env (R7RS), then binding.
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return single (fail (sc, "gf0: bad let-values", x));
    Env inner= push_frame (env);
    for (pointer cs= gf::car (rest); gf::is_pair (cs); cs= gf::cdr (cs)) {
      pointer c= gf::car (cs);
      if (!gf::is_pair (c) || !gf::is_pair (gf::cdr (c)) ||
          gf::is_pair (gf::cdr (gf::cdr (c))))
        return single (fail (sc, "gf0: bad let-values clause", x));
      V iv= eval (sc, gf::car (gf::cdr (c)), env);
      if (!iv.multi) {
        std::vector<pointer> one;
        one.push_back (iv.one);
        iv= multi_vec (std::move (one));
      }
      bind_formals (sc, gf::car (c), args_to_list (sc, iv.many),
                    inner.frames->back ()->bindings, x);
    }
    V r= single (gf::unspecified (sc));
    for (pointer b= gf::cdr (rest); gf::is_pair (b); b= gf::cdr (b))
      r= eval (sc, gf::car (b), inner);
    return r;
  }
  if (is_head (x, "values")) {
    std::vector<pointer> out;
    pointer tail= gf::cdr (x);
    for (; gf::is_pair (tail); tail= gf::cdr (tail))
      out.push_back (pin (sc, must_single (sc, eval (sc, gf::car (tail), env),
                                           x, "gf0: values element must be single")));
    if (!gf::is_null (sc, tail))
      return single (fail (sc, "gf0: improper values", x));
    return multi_vec (std::move (out));
  }
  if (is_head (x, "call-with-values")) {
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || !gf::is_pair (gf::cdr (rest)) ||
        gf::is_pair (gf::cdr (gf::cdr (rest))))
      return single (fail (sc, "gf0: bad call-with-values", x));
    pointer producer= must_single (sc, eval (sc, gf::car (rest), env), x,
                                      "gf0: cwv producer must be single-valued");
    // R7RS: the producer is CALLED with zero args; its values feed consumer.
    V pv= apply_values (sc, producer, std::vector<pointer> (), x);
    std::vector<pointer> got;
    if (pv.multi) got= pv.many;
    else got.push_back (pv.one);
    pointer consumer= must_single (sc, eval (sc, gf::car (gf::cdr (rest)), env), x,
                                   "gf0: cwv consumer must be single-valued");
    return apply_values (sc, consumer, got, x);
  }
  // Call: (proc args...).
  if (is_frontend_syntax (gf::car (x), env))
    return single (fail (sc, "gf0: not core; desugar via frontend", gf::car (x)));
  pointer proc= must_single (sc, eval (sc, gf::car (x), env), x,
                             "gf0: call head must be single-valued");
  std::vector<pointer> argv;
  evlis_single (sc, gf::cdr (x), env, argv);
  return apply_values (sc, proc, argv, x);
}

static void
ensure_top (scheme* sc) {
  if (s_top.frames == nullptr) {
    s_top.frames= std::make_shared<std::vector<std::shared_ptr<Frame>>> ();
    s_top.frames->push_back (std::make_shared<Frame> ());
  }
  (void) sc;
}

static gf::pointer
f_gf0_apply (scheme* sc, pointer args) {
  pointer box= gf::car (args);
  pointer tail= gf::cdr (args);
  if (!gf::is_pair (tail) || gf::is_pair (gf::cdr (tail)))
    return fail (sc, "gf0: g_gf0-apply takes (box arglist)", args);
  pointer arglist= gf::car (tail);
  if (s_boxes.find ((void*) box) == s_boxes.end ())
    return fail (sc, "gf0: stale closure", box);
  gf::int_ idx= (gf::int_) (intptr_t) gf::c_object_value (box);
  if (idx < 0 || (size_t) idx >= s_registry.size ())
    return fail (sc, "gf0: stale closure", box);
  V r= apply_closure (sc, s_registry[(size_t) idx], arglist, box);
  return must_single (sc, r, box, "gf0: s7 callback must be single-valued");
}

static gf::pointer
f_gf0_eval (scheme* sc, pointer args) {
  ensure_top (sc);
  V r= eval (sc, gf::car (args), s_top);
  if (!r.multi) return r.one;
  // Boundary multi TBD (M-VM): s7's spread protocol (splice_in_values
  // stack-op dance) does not trigger for C-returned s7_values objects, so
  // the reference boundary stays single-valued and strict (R7RS-like:
  // multi in single position is an error). Collect with g_gf0-eval-values.
  return fail (sc, "gf0: multi-valued at single boundary; use g_gf0-eval-values",
               gf::car (args));
}

// Seed the session top env from an s7 inlet (e.g. the-expander-library):
// compiled artifacts reference library bindings by gensym, which only
// resolve there. Copies (name . value) cells by reference (pinned); gf0
// set! writes its own frames, never back into the inlet. M2a differential
// bridge; toplevel cells (M2) retire it.
static gf::pointer
f_gf0_import_inlet (scheme* sc, pointer args) {
  ensure_top (sc);
  pointer inlet= gf::car (args);
  pointer alist= gf::let_to_list (sc, inlet);
  for (; gf::is_pair (alist); alist= gf::cdr (alist)) {
    pointer e= gf::car (alist);
    pointer sym= nullptr;
    pointer val= nullptr;
    if (gf::is_pair (e) && gf::is_symbol (gf::car (e))) {
      sym= gf::car (e);
      pointer tail= gf::cdr (e);
      val= gf::is_pair (tail) ? gf::car (tail) : tail;
    }
    if (sym != nullptr)
      s_top.frames->back ()->bindings.push_back ({pin (sc, sym), pin (sc, val)});
  }
  return gf::unspecified (sc);
}

static gf::pointer
f_gf0_eval_values (scheme* sc, pointer args) {
  ensure_top (sc);
  V r= eval (sc, gf::car (args), s_top);
  if (!r.multi) {
    std::vector<pointer> one;
    one.push_back (r.one);
    return args_to_list (sc, one);
  }
  return args_to_list (sc, r.many);
}

} // namespace gf0

void
glue_gf0_eval (gf::scheme* sc) {
  gf::define_function (sc, "g_gf0-eval", gf0::f_gf0_eval, 1, 0, false,
                       "(g_gf0-eval datum) => value, single-valued reference eval (multi is an error)");
  gf::define_function (sc, "g_gf0-eval-values", gf0::f_gf0_eval_values, 1, 0, false,
                       "(g_gf0-eval-values datum) => list of values from reference eval");
  gf::define_function (sc, "g_gf0-import-inlet", gf0::f_gf0_import_inlet, 1, 0, false,
                       "(g_gf0-import-inlet inlet) => unspecified, seed gf0 session env from an s7 inlet");
  gf::define_function (sc, "g_gf0-apply", gf0::f_gf0_apply, 2, 0, false,
                       "(g_gf0-apply box arglist) => value, apply a gf0 closure box (s7 callback entry)");
}

} // namespace goldfish
