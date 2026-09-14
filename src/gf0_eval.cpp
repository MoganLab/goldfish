//
// gf0_eval.cpp -- reference evaluator for lowered core scheme (T0-ahead).
//
// The first permanent brick of the s7 replacement: a tree-walking evaluator
// for `ir->core` output (CORE-SEMANTICS.md), seeing ONLY the gf:: surface
// (this TU must never include s7.h nor call s7_*; lint enforces it).
// s7 stays as reader/object model; gf0 owns env frames + closures.
//
// Evaluation is a trampoline (M-VM-1): step() resolves one level to either
// a value or Resume{env, body}; finish()/step_seq() drive Resume flat, so
// tail calls never nest C++ frames (proper TCO). Only statically-nested
// non-tail positions (tests, inits, operands) recurse, bounded by source
// nesting; the C-stack guard remains for those.
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
// Errors are GfEx (C++ exception through gf0 frames only): fail() throws,
// s7 errors convert at the s7call boundary (catch-wrap + marker), catch/
// error/throw work natively, and every s7->gf0 entry converts back to
// gf::error. A C++ exception never crosses s7 C frames.
//
// Entry: (g_gf0-eval datum) evaluates one lowered datum in the session top
// env (defines persist across datums of one process).
// Driver: `gf eval-gf0 CODE` reads data forms, prints each result as a list
// (call-with-values + list), so multi-values display uniformly.
//

#include "gf.h"

#include <cstdint>
#include <cstring>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>
#if defined(__linux__)
#include <pthread.h>
#include <sys/resource.h>
#endif

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
static gf::int_                  s_closure_type= -1;
static gf::int_                  s_unassigned_type= -1;

static pointer
pin (scheme* sc, pointer p) {
  gf::gc_protect (sc, p);
  return p;
}

// Engine exception (M-VM-2a): unwound through gf0 C++ frames with
// destructors running (unlike longjmp). NEVER crosses s7 C frames --
// every s7->gf0 entry (f_gf0_*, trampoline) converts to gf::error, and
// every gf0->s7 call (s7call_vec) converts s7 errors back at its boundary.
struct GfEx {
  std::vector<pointer> args; // handler-visible: [key, info...]
};
static pointer s_raised_marker= nullptr; // pinned identity token, not a primitive

static pointer
fail (scheme* sc, const char* msg, pointer irritant) {
  GfEx e;
  e.args.push_back (pin (sc, gf::make_symbol (sc, "gf0-error")));
  e.args.push_back (pin (sc, gf::make_string (sc, msg)));
  e.args.push_back (pin (sc, irritant));
  throw e;
}

static gf::int_
c_type_cached (scheme* sc, const char* name, gf::int_* slot) {
  // make_c_type mints a fresh tag per call; cache one tag per box kind
  // (100k lambdas must not mint 100k type tags).
  if (*slot == -1) *slot= gf::make_c_type (sc, name);
  return *slot;
}

#if defined(__linux__)
// Exact C-stack guard: tree-walking without TCO overflows ~900 nested
// applies at 8MB (measured). Fail cleanly instead of segfaulting; proper
// tail calls are M-VM (engine-owned control stack).
static void*  s_stack_base= nullptr;
static size_t s_stack_size= 0;
static void
stack_init_once () {
  if (s_stack_base == nullptr) {
    pthread_attr_t attr;
    if (pthread_getattr_np (pthread_self (), &attr) == 0) {
      pthread_attr_getstack (&attr, &s_stack_base, &s_stack_size);
      pthread_attr_destroy (&attr);
    }
  }
  if (s_stack_base == nullptr) {
    // Fallback: rlimit size + a near-top anchor captured here (init runs
    // near the top of the main stack). Conservative: fires early, never late.
    struct rlimit rl;
    if (getrlimit (RLIMIT_STACK, &rl) == 0 && rl.rlim_cur != RLIM_INFINITY) {
      char anchor;
      s_stack_base= (void*) ((uintptr_t) &anchor - (uintptr_t) rl.rlim_cur);
      s_stack_size= (size_t) rl.rlim_cur;
    }
  }
}
static bool
stack_low () {
  if (s_stack_base == nullptr) return false;
  char here;
  uintptr_t sp= (uintptr_t) &here;
  return sp < (uintptr_t) s_stack_base + (uintptr_t) (1 << 20);
}
#else
static void stack_init_once () {}
static bool stack_low () { return false; }
#endif

static pointer
unassigned_box (scheme* sc) {
  if (s_unassigned == nullptr) {
    // NOTE: the value slot must hold a REAL s7 object (here an integer),
    // and the let slot a REAL let (rootlet): s7's GC marks both, and
    // fake pointers / nil here segfault the marker on first collection
    // (M2a deep-recursion crash).
    s_unassigned= pin (sc, gf::make_c_object_with_let (
                           sc, c_type_cached (sc, "gf0-unassigned", &s_unassigned_type),
                           (void*) pin (sc, gf::make_integer (sc, -1)),
                           gf::rootlet (sc)));
  }
  return s_unassigned;
}

static bool
is_head (pointer x, const char* name) {
  return gf::is_pair (x) && gf::is_symbol (gf::car (x)) &&
         std::strcmp (gf::symbol_name (gf::car (x)), name) == 0;
}

// One evaluation step: either a resolved value, or Resume{env, body} asking
// the driver loop to evaluate a body sequence (last in tail position).
// Tail calls never nest C++ frames; only statically-nested non-tail
// positions (tests, inits, operands) recurse via eval().
struct Step {
  bool    resume;
  V       v;
  Env     renv;
  pointer rbody;
};
static Step
val (V v) {
  Step s;
  s.resume= false;
  s.v     = v;
  return s;
}
static Step
go (scheme* sc, Env env, pointer body) {
  Step s;
  s.resume= true;
  s.renv  = env;
  s.rbody = pin (sc, body);
  return s;
}

static V eval (scheme* sc, pointer x, Env env);
static V apply_values (scheme* sc, pointer proc, const std::vector<pointer>& many,
                       pointer ctx);

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
  if (s_raised_marker == nullptr)
    s_raised_marker= pin (sc, gf::make_symbol (sc, "gf0-raised"));
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
  // Catch s7 errors AT this boundary and convert to GfEx (a longjmp here
  // would fly past gf0 frames to the wrong catcher). The handler spreads
  // (MARKER . handler-args) as multiple values, so a raised call collects
  // with MARKER in head position, unambiguous against value shapes.
  pointer inner= pin (sc, gf::list (sc,
    pin (sc, gf::make_symbol (sc, "catch")),
    gf::t (sc),
    thunk,
    pin (sc, gf::list (sc,
      pin (sc, gf::make_symbol (sc, "lambda")),
      pin (sc, gf::make_symbol (sc, "hargs")),
      pin (sc, gf::list (sc,
        pin (sc, gf::make_symbol (sc, "apply")),
        pin (sc, gf::make_symbol (sc, "values")),
        pin (sc, gf::list (sc,
          pin (sc, gf::make_symbol (sc, "cons")),
          pin (sc, gf::list (sc, quote_sym, s_raised_marker)),
          pin (sc, gf::make_symbol (sc, "hargs"))))))))));
  pointer outer= pin (sc, gf::list (sc,
    pin (sc, gf::make_symbol (sc, "lambda")),
    gf::nil (sc), inner));
  pointer expr= pin (sc, gf::list (sc, pin (sc, gf::make_symbol (sc, "call-with-values")),
                                   outer, pin (sc, gf::make_symbol (sc, "list"))));
  pointer collected= gf::eval (sc, expr, gf::rootlet (sc));
  std::vector<pointer> out;
  pointer tail= collected;
  for (; gf::is_pair (tail); tail= gf::cdr (tail))
    out.push_back (pin (sc, gf::car (tail)));
  if (!gf::is_null (sc, tail))
    return multi_vec (std::vector<pointer>{fail (sc, "gf0: improper collection", collected)});
  if (!out.empty () && out[0] == s_raised_marker) {
    GfEx e;
    for (size_t i= 1; i < out.size (); ++i) e.args.push_back (out[i]);
    throw e;
  }
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

// Bind a closure call (no body evaluation): the Resume driver finishes it.
static Env
bind_call (scheme* sc, const Closure& c, pointer arglist, pointer ctx) {
  Env                   inner= push_frame (c.env);
  std::vector<Binding>& frame= inner.frames->back ()->bindings;
  bind_formals (sc, c.formals, arglist, frame, ctx);
  return inner;
}

// Evaluate a body sequence to its last value (empty -> unspecified).
// Prefix forms resolve fully via nested eval; the last form steps flat.
static Step step (scheme* sc, pointer x, Env env);
static Step
step_seq (scheme* sc, Env env, pointer body) {
  if (!gf::is_pair (body)) {
    Step s= val (single (gf::unspecified (sc)));
    return s;
  }
  for (; gf::is_pair (gf::cdr (body)); body= gf::cdr (body))
    eval (sc, gf::car (body), env);
  return step (sc, gf::car (body), env);
}

// Drive Resume continuations flat; V positions resolve inside.
static V
finish (scheme* sc, Step s) {
  while (s.resume) s= step_seq (sc, s.renv, s.rbody);
  return s.v;
}

static V
eval (scheme* sc, pointer x, Env env) {
  return finish (sc, step (sc, x, env));
}

static V
apply_values (scheme* sc, pointer proc, const std::vector<pointer>& many, pointer ctx) {
  if (s_boxes.find ((void*) proc) != s_boxes.end ()) {
    gf::int_ idx= gf::integer ((pointer) gf::c_object_value (proc));
    if (idx < 0 || (size_t) idx >= s_registry.size ())
      return single (fail (sc, "gf0: stale closure", proc));
    const Closure& c= s_registry[(size_t) idx];
    return finish (sc, step_seq (sc, bind_call (sc, c, args_to_list (sc, many), ctx),
                                 c.body));
  }
  if (gf::is_procedure (proc)) return s7call_vec (sc, proc, many);
  return single (fail (sc, "gf0: cannot apply (macros stay frontend)", proc));
}

static Step
step (scheme* sc, pointer x, Env env) {
  if (stack_low ())
    return val (single (fail (sc, "gf0: C stack low (no TCO yet; see M-VM)", x)));
  // Self-evaluating.
  if (gf::is_boolean (x) || gf::is_number (x) || gf::is_string (x) ||
      gf::is_character (x) || gf::is_null (sc, x) || gf::is_vector (x))
    return val (single (x));
  if (gf::is_symbol (x)) {
    pointer v= lookup_raw (sc, x, env);
    if (v == unassigned_box (sc))
      return val (single (fail (sc, "gf0: read before assignment (R7RS letrec)", x)));
    return val (single (v));
  }
  if (!gf::is_pair (x)) return val (single (fail (sc, "gf0: cannot evaluate", x)));

  if (is_head (x, "quote")) {
    if (!gf::is_pair (gf::cdr (x)) || gf::is_pair (gf::cdr (gf::cdr (x))))
      return val (single (fail (sc, "gf0: bad quote", x)));
    return val (single (gf::car (gf::cdr (x))));
  }
  if (is_head (x, "if")) {
    // NOTE: s7 nil is a live pointer, never nullptr; shape checks must use
    // is_pair/is_null, not pointer comparison.
    pointer rest1= gf::cdr (x);
    if (!gf::is_pair (rest1)) return val (single (fail (sc, "gf0: bad if", x)));
    pointer rest2= gf::cdr (rest1);
    if (!gf::is_pair (rest2)) return val (single (fail (sc, "gf0: bad if", x)));
    pointer rest3= gf::cdr (rest2);
    if (!(gf::is_null (sc, rest3) ||
          (gf::is_pair (rest3) && gf::is_null (sc, gf::cdr (rest3)))))
      return val (single (fail (sc, "gf0: bad if", x)));
    pointer t= must_single (sc, eval (sc, gf::car (rest1), env), x,
                            "gf0: if test must be single-valued");
    if (!gf::boolean (sc, t)) {
      if (!gf::is_pair (rest3)) {
        Step s= val (single (gf::unspecified (sc)));
        return s;
      }
      return go (sc, env, gf::list (sc, gf::car (rest3)));
    }
    return go (sc, env, gf::list (sc, gf::car (rest2)));
  }
  if (is_head (x, "begin")) {
    return go (sc, env, gf::cdr (x));
  }
  if (is_head (x, "lambda")) {
    if (!gf::is_pair (gf::cdr (x)))
      return val (single (fail (sc, "gf0: bad lambda", x)));
    Closure c;
    c.formals= pin (sc, gf::car (gf::cdr (x)));
    c.body   = pin (sc, gf::cdr (gf::cdr (x)));
    c.env    = env;
    s_registry.push_back (c);
    gf::int_ idx= (gf::int_) (s_registry.size () - 1);
    pointer  box= pin (sc, gf::make_c_object_with_let (
                         sc, c_type_cached (sc, "gf0-closure", &s_closure_type),
                         (void*) pin (sc, gf::make_integer (sc, idx)),
                         gf::rootlet (sc)));
    s_boxes.insert ((void*) box);
    return val (single (box));
  }
  if (is_head (x, "define")) {
    pointer rest= gf::cdr (x);
    bool ok= gf::is_pair (rest) && gf::is_symbol (gf::car (rest)) &&
             gf::is_pair (gf::cdr (rest)) &&
             gf::is_null (sc, gf::cdr (gf::cdr (rest)));
    if (!ok)
      return val (single (fail (sc, "gf0: only (define name exp); sugar stays frontend", x)));
    pointer name= gf::car (rest);
    pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (rest)), env), x,
                                     "gf0: define init must be single-valued"));
    env.frames->back ()->bindings.push_back ({pin (sc, name), v});
    return val (single (gf::unspecified (sc)));
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
        return val (single (fail (sc, "gf0: bad module-set", x)));
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
        return val (single (fail (sc, "gf0: module-ref unavailable (boot substrate?)", x)));
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
      return val (single (gf::unspecified (sc)));
    }
    bool ok= gf::is_pair (rest) && gf::is_symbol (gf::car (rest)) &&
             gf::is_pair (gf::cdr (rest)) &&
             gf::is_null (sc, gf::cdr (gf::cdr (rest)));
    if (!ok) return val (single (fail (sc, "gf0: bad set!", x)));
    pointer name= gf::car (rest);
    pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (rest)), env), x,
                                     "gf0: set! value must be single-valued"));
    const char* want= gf::symbol_name (name);
    for (size_t i= env.frames->size (); i-- > 0;) {
      std::vector<Binding>& bs= (*env.frames)[i]->bindings;
      for (size_t j= bs.size (); j-- > 0;) {
        if (std::strcmp (gf::symbol_name (bs[j].sym), want) == 0) {
          bs[j].val= v;
          return val (single (gf::unspecified (sc)));
        }
      }
    }
    if (gf::is_defined (sc, want)) { // s7 toplevel cell (scaffolding, M2)
      gf::define (sc, gf::rootlet (sc), name, v);
      return val (single (gf::unspecified (sc)));
    }
    return val (single (fail (sc, "gf0: set! of unbound variable", name)));
  }
  if (is_head (x, "let")) {
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return val (single (fail (sc, "gf0: bad let", x)));
    Env inner= push_frame (env);
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return val (single (fail (sc, "gf0: bad let binding", x)));
      pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (b)), env), x,
                                       "gf0: let init must be single-valued"));
      inner.frames->back ()->bindings.push_back ({pin (sc, gf::car (b)), v});
    }
    return go (sc, inner, gf::cdr (rest));
  }
  if (is_head (x, "let*")) {
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return val (single (fail (sc, "gf0: bad let*", x)));
    Env inner= push_frame (env);
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return val (single (fail (sc, "gf0: bad let* binding", x)));
      pointer v= pin (sc, must_single (sc, eval (sc, gf::car (gf::cdr (b)), inner), x,
                                       "gf0: let* init must be single-valued"));
      inner.frames->back ()->bindings.push_back ({pin (sc, gf::car (b)), v});
    }
    return go (sc, inner, gf::cdr (rest));
  }
  if (is_head (x, "letrec") || is_head (x, "letrec*")) {
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return val (single (fail (sc, "gf0: bad letrec", x)));
    Env inner= push_frame (env);
    std::vector<Binding>& frame= inner.frames->back ()->bindings;
    // Validate + pre-allocate all slots unassigned (R7RS: init-period reads
    // of any slot are an error; the reader reports the slot, not #<undefined>).
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return val (single (fail (sc, "gf0: bad letrec binding", x)));
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
    return go (sc, inner, gf::cdr (rest));
  }
  if (is_head (x, "let-values")) {
    // Lowered code already has call-with-values; accept raw let-values by
    // evaluating each init in the OUTER env (R7RS), then binding.
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return val (single (fail (sc, "gf0: bad let-values", x)));
    Env inner= push_frame (env);
    for (pointer cs= gf::car (rest); gf::is_pair (cs); cs= gf::cdr (cs)) {
      pointer c= gf::car (cs);
      if (!gf::is_pair (c) || !gf::is_pair (gf::cdr (c)) ||
          gf::is_pair (gf::cdr (gf::cdr (c))))
        return val (single (fail (sc, "gf0: bad let-values clause", x)));
      V iv= eval (sc, gf::car (gf::cdr (c)), env);
      if (!iv.multi) {
        std::vector<pointer> one;
        one.push_back (iv.one);
        iv= multi_vec (std::move (one));
      }
      bind_formals (sc, gf::car (c), args_to_list (sc, iv.many),
                    inner.frames->back ()->bindings, x);
    }
    return go (sc, inner, gf::cdr (rest));
  }
  if (is_head (x, "values")) {
    std::vector<pointer> out;
    pointer tail= gf::cdr (x);
    for (; gf::is_pair (tail); tail= gf::cdr (tail))
      out.push_back (pin (sc, must_single (sc, eval (sc, gf::car (tail), env),
                                           x, "gf0: values element must be single")));
    if (!gf::is_null (sc, tail))
      return val (single (fail (sc, "gf0: improper values", x)));
    return val (multi_vec (std::move (out)));
  }
  if (is_head (x, "catch")) {
    // Native s7-protocol catch (guard lowers to this): (catch TAG THUNK HANDLER).
    // s7 errors inside surface as GfEx at the s7call boundary, so this try
    // sees both gf0 and s7 errors uniformly. Non-matching keys rethrow.
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || !gf::is_pair (gf::cdr (rest)) ||
        !gf::is_pair (gf::cdr (gf::cdr (rest))) ||
        gf::is_pair (gf::cdr (gf::cdr (gf::cdr (rest)))))
      return val (single (fail (sc, "gf0: bad catch", x)));
    pointer tag= must_single (sc, eval (sc, gf::car (rest), env), x,
                              "gf0: catch tag must be single-valued");
    pointer thunk= must_single (sc, eval (sc, gf::car (gf::cdr (rest)), env), x,
                                "gf0: catch thunk must be single-valued");
    pointer handler= must_single (sc, eval (sc, gf::car (gf::cdr (gf::cdr (rest))), env),
                                  x, "gf0: catch handler must be single-valued");
    try {
      return val (apply_values (sc, thunk, std::vector<pointer> (), x));
    }
    catch (GfEx& e) {
      bool all= gf::is_boolean (tag) && gf::boolean (sc, tag);
      if (!all && (e.args.empty () || !gf::is_eq (tag, e.args[0])))
        throw;
      return val (apply_values (sc, handler, e.args, x));
    }
  }
  if (is_head (x, "call-with-values")) {
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || !gf::is_pair (gf::cdr (rest)) ||
        gf::is_pair (gf::cdr (gf::cdr (rest))))
      return val (single (fail (sc, "gf0: bad call-with-values", x)));
    pointer producer= must_single (sc, eval (sc, gf::car (rest), env), x,
                                      "gf0: cwv producer must be single-valued");
    // R7RS: the producer is CALLED with zero args; its values feed consumer.
    V pv= apply_values (sc, producer, std::vector<pointer> (), x);
    std::vector<pointer> got;
    if (pv.multi) got= pv.many;
    else got.push_back (pv.one);
    pointer consumer= must_single (sc, eval (sc, gf::car (gf::cdr (rest)), env), x,
                                   "gf0: cwv consumer must be single-valued");
    if (s_boxes.find ((void*) consumer) != s_boxes.end ()) {
      gf::int_ idx= gf::integer ((pointer) gf::c_object_value (consumer));
      if (idx < 0 || (size_t) idx >= s_registry.size ())
        return val (single (fail (sc, "gf0: stale closure", consumer)));
      const Closure& c= s_registry[(size_t) idx];
      return go (sc, bind_call (sc, c, args_to_list (sc, got), x), c.body);
    }
    return val (apply_values (sc, consumer, got, x));
  }
  // Call: (proc args...).
  if (is_frontend_syntax (gf::car (x), env))
    return val (single (fail (sc, "gf0: not core; desugar via frontend", gf::car (x))));
  pointer proc= must_single (sc, eval (sc, gf::car (x), env), x,
                             "gf0: call head must be single-valued");
  std::vector<pointer> argv;
  evlis_single (sc, gf::cdr (x), env, argv);
  if (s_boxes.find ((void*) proc) != s_boxes.end ()) {
    gf::int_ idx= gf::integer ((pointer) gf::c_object_value (proc));
    if (idx < 0 || (size_t) idx >= s_registry.size ())
      return val (single (fail (sc, "gf0: stale closure", proc)));
    const Closure& c= s_registry[(size_t) idx];
    return go (sc, bind_call (sc, c, args_to_list (sc, argv), x), c.body);
  }
  return val (apply_values (sc, proc, argv, x));
}

// s7->gf0 entries convert GfEx back to gf::error (a C++ exception must
// never cross s7 C frames). Shape preserves fail() rendering exactly:
// error(args[0], rest-as-list).
static pointer
gfex_to_error (scheme* sc, GfEx& e) {
  pointer type= e.args.empty () ? gf::make_symbol (sc, "gf0-error") : e.args[0];
  std::vector<pointer> rest;
  for (size_t i= 1; i < e.args.size (); ++i) rest.push_back (e.args[i]);
  return gf::error (sc, type, args_to_list (sc, rest));
}

static void
ensure_top (scheme* sc) {
  stack_init_once ();
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
  gf::int_ idx= gf::integer ((pointer) gf::c_object_value (box));
  if (idx < 0 || (size_t) idx >= s_registry.size ())
    return fail (sc, "gf0: stale closure", box);
  try {
    const Closure& c= s_registry[(size_t) idx];
    V r= finish (sc, step_seq (sc, bind_call (sc, c, arglist, box), c.body));
    return must_single (sc, r, box, "gf0: s7 callback must be single-valued");
  }
  catch (GfEx& e) {
    return gfex_to_error (sc, e);
  }
}

static gf::pointer
f_gf0_eval (scheme* sc, pointer args) {
  ensure_top (sc);
  try {
    V r= eval (sc, gf::car (args), s_top);
    if (!r.multi) return r.one;
    // Boundary multi TBD (M-VM): s7's spread protocol (splice_in_values
    // stack-op dance) does not trigger for C-returned s7_values objects, so
    // the reference boundary stays single-valued and strict (R7RS-like:
    // multi in single position is an error). Collect with g_gf0-eval-values.
    return fail (sc, "gf0: multi-valued at single boundary; use g_gf0-eval-values",
                 gf::car (args));
  }
  catch (GfEx& e) {
    return gfex_to_error (sc, e);
  }
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
  try {
    V r= eval (sc, gf::car (args), s_top);
    if (!r.multi) {
      std::vector<pointer> one;
      one.push_back (r.one);
      return args_to_list (sc, one);
    }
    return args_to_list (sc, r.many);
  }
  catch (GfEx& e) {
    return gfex_to_error (sc, e);
  }
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
