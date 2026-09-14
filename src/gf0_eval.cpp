//
// gf0_eval.cpp -- reference evaluator for lowered core scheme (T0-ahead).
//
// The first permanent brick of the s7 replacement: a tree-walking evaluator
// for `ir->core` output (CORE-SEMANTICS.md), seeing ONLY the gf:: surface
// (this TU must never include s7.h nor call s7_*; lint enforces it).
// s7 stays as reader/object model; gf0 owns env frames + closures.
//
// Evaluation is CEK (M-VM-2b): stepE() resolves one level to a value or
// pushes an explicit Kont frame; runLoop() drives Control flat, so tail
// calls never nest C++ frames (proper TCO) and continuations are capturable
// (capture = copy Kont; invoke = install copy). Only s7call leaves nest
// (bounded by host); the C-stack guard remains for those paths.
// call/cc is native multi-shot; dynamic-wind is an explicit stub.
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
// Known limits (by design, not bugs): no unprotect (all held pointers are
//   pinned for the session; debug-scale only), symbols compared by name
//   (interning-robust). Toplevel cells are gf0-owned since M2 (session top
//   frame seeded once from the rootlet; later s7-side definitions invisible
//   by design). TCO via the CEK loop; C-stack guard kept for s7call leaves.
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
#include <unordered_map>
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
  // Roots travel with values: shared ownership, unprotect when the last
  // holder dies. Stops the pin-everything leak (M3 set-size: 1M-element
  // loops pinned ~40 objects/iteration with no release -> 12GB OOM).
  // A null roots means "externally rooted" (s7-stack args, datum subforms,
  // frame/env/registry-held values); every holder must ensure one of these.
  struct PinData {
    scheme* sc;
    std::vector<int> locs;
    ~PinData () {
      for (size_t i= locs.size (); i-- > 0;)
        gf::gc_unprotect_at (sc, locs[i]);
    }
  };
  std::shared_ptr<PinData> roots;
};
using Roots = std::shared_ptr<V::PinData>;
static Roots
make_roots (scheme* sc) {
  Roots r= std::make_shared<V::PinData> ();
  r->sc= sc;
  return r;
}
static pointer
keep_in (Roots& r, scheme* sc, pointer p) {
  if (!r) r= make_roots (sc);
  r->locs.push_back (gf::gc_protect (sc, p));
  return p;
}
static V
single (scheme* sc, pointer p) {
  V v;
  v.multi= false;
  v.one  = p;
  keep_in (v.roots, sc, p);
  return v;
}
static V
multi_vec (scheme* sc, std::vector<pointer>&& m) {
  V v;
  v.multi= true;
  v.many = std::move (m);
  for (pointer p : v.many) keep_in (v.roots, sc, p);
  return v;
}
// NOTE: single/multi_vec REQUIRE sc (roots travel with values). No no-sc
// overloads exist on purpose: unrooted Vs dangle across nested s7 evals.

// Persistent lexical env: frames[0] is outermost; innermost last. Each
// frame is individually shared so set! through any captured env mutates the
// one frame object every holder sees.
struct Binding {
  pointer sym;
  pointer val;
};
struct Frame {
  std::vector<Binding> bindings;
  // Roots for member values (shared with captures); unprotect when the
  // last holder (frame users + Kont copies + captures) dies.
  std::vector<Roots> roots;
};
struct Env {
  std::shared_ptr<std::vector<std::shared_ptr<Frame>>> frames;
};
// Bind name<-value(single) into a frame, rooting both for frame lifetime.
static void
frame_bind (scheme* sc, std::shared_ptr<Frame> fr, pointer name, V v) {
  Roots r= make_roots (sc);
  keep_in (r, sc, name);
  if (v.roots) {
    // share the value's roots (no double-protect bookkeeping needed)
    fr->roots.push_back (v.roots);
  }
  else {
    keep_in (r, sc, v.one);
  }
  fr->roots.push_back (r);
  fr->bindings.push_back ({name, v.one});
}
struct Closure {
  pointer formals;
  pointer body; // list of body exprs
  Env     env;
};

static std::vector<Closure>      s_registry;
static std::unordered_set<void*> s_boxes; // c_object boxes holding a registry index
static std::unordered_map<void*, pointer> s_wrap_fwd; // box -> wrapper (identity)
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
  Roots roots;               // roots for args (shared across copies)
};
static pointer s_raised_marker= nullptr; // pinned identity token, not a primitive

static pointer
fail_key (scheme* sc, const char* key, const char* msg, pointer irritant) {
  GfEx e;
  e.args.push_back (pin (sc, gf::make_symbol (sc, key)));
  e.args.push_back (pin (sc, gf::make_string (sc, msg)));
  e.args.push_back (pin (sc, irritant));
  throw e;
}

static pointer
fail (scheme* sc, const char* msg, pointer irritant) {
  return fail_key (sc, "gf0-error", msg, irritant);
}

static gf::pointer gf0_box_ref (gf::scheme* sc, gf::pointer args);

static gf::int_
c_type_cached (scheme* sc, const char* name, gf::int_* slot) {
  // make_c_type mints a fresh tag per call; cache one tag per box kind
  // (100k lambdas must not mint 100k type tags).
  if (*slot == -1) {
    *slot= gf::make_c_type (sc, name);
    // SPIKE (M3 interop): applicable boxes. ref fires when a box occurs
    // in function position; unassigned boxes stay inapplicable.
    if (std::strcmp (name, "gf0-unassigned") != 0)
      gf::c_type_set_ref (sc, *slot, gf0_box_ref);
  }
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

// User shadowing check: frame 0 is the host snapshot (M2 seed), never
// user code. Seeded host names (call/cc, let, ...) must NOT count as
// shadowed, or the syntax/call-cc gates misfire (M2 generator regress).
static bool
user_bound (pointer sym, Env env) {
  for (size_t i= env.frames->size (); i-- > 0;) {
    if (i == 0) continue;
    if (frame_has (sym, (*env.frames)[i])) return true;
  }
  return false;
}

static bool
is_frontend_syntax (pointer sym, Env env) {
  if (!gf::is_symbol (sym) || user_bound (sym, env)) return false;
  const char* want= gf::symbol_name (sym);
  for (const char** k= kFrontendSyntax; *k; ++k)
    if (std::strcmp (want, *k) == 0) return true;
  return false;
}

static V eval (scheme* sc, pointer x, Env env);

static pointer
args_to_list (scheme* sc, const std::vector<pointer>& argv);

static V
must_single (scheme* sc, V v, pointer ctx, const char* what) {
  if (v.multi)
    fail_key (sc, "wrong-number-of-args", what, ctx); // throws; return keeps form
  return v;
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
  // M2: no live rootlet fallback. The session top frame is seeded from the
  // rootlet once (ensure_top); anything not found is unbound, even if s7
  // defines it later. Engines diverge by design from here on. Key matches
  // s7 (check-catch compares keys).
  return fail_key (sc, "unbound-variable", "gf0: unbound variable", sym);
}

// s7 call with uniform multi collection: (call-with-values
// (lambda () (PROC 'A ...)) list) always yields a proper list, even for
// 0 results. Quoting is value-exact for every type (symbols included).
// gf0 closures crossing into s7 are wrapped in a trampoline lambda that
// calls back via g_gf0-apply (s7 cannot apply c_object boxes; callbacks
// must be single-valued).
static pointer
wrap_for_s7 (scheme* sc, pointer box) {
  Roots tmp;
  pointer quote_sym= keep_in (tmp, sc, gf::make_symbol (sc, "quote"));
  // (lambda args (apply values (g_gf0-apply-values 'BOX args))): the
  // values-variant spreads multi through s7 natively, so callbacks keep
  // SRFI multi propagation (e.g. set-search! success/failure results).
  pointer inner= keep_in (tmp, sc, gf::list (sc,
    keep_in (tmp, sc, gf::make_symbol (sc, "g_gf0-apply-values")),
    keep_in (tmp, sc, gf::list (sc, quote_sym, box)),
    keep_in (tmp, sc, gf::make_symbol (sc, "args"))));
  pointer call= keep_in (tmp, sc, gf::list (sc,
    keep_in (tmp, sc, gf::make_symbol (sc, "apply")),
    keep_in (tmp, sc, gf::make_symbol (sc, "values")),
    inner));
  pointer expr= keep_in (tmp, sc, gf::list (sc,
    keep_in (tmp, sc, gf::make_symbol (sc, "lambda")),
    keep_in (tmp, sc, gf::make_symbol (sc, "args")),
    call));
  return pin (sc, gf::eval (sc, expr, gf::rootlet (sc)));
}

static pointer wrap_box (scheme* sc, pointer box);

static void hof_maybe_wrap (scheme* sc, pointer proc, std::vector<pointer>& argv);

// s7call token stack (stale-continuation fence): every gf0->s7 crossing
// pushes a fresh token, popped by RAII (exception-safe: GfEx flies through
// here). The s7-side guarded call/cc (installed by g_gf0-import-inlet)
// stamps captures and refuses to invoke across a return boundary with an
// explicit gf0-stale-continuation error instead of longjmp-ing into a dead
// C++ frame (srfi-158 coroutines). Equality against the CURRENT innermost
// token is nest-correct: invoke-inside still matches, invoke-after differs.
static gf::int_ s_s7call_next= 1;
static std::vector<gf::int_> s_s7call_stack;
struct S7CallToken {
  S7CallToken () {
    s_s7call_stack.push_back (s_s7call_next++);
  }
  ~S7CallToken () {
    s_s7call_stack.pop_back ();
  }
};

static V
s7call_vec (scheme* sc, pointer proc, const std::vector<pointer>& argv) {
  S7CallToken fence;
  // Everything touched here is scope-rooted for the call, so callers need
  // no pin discipline: argv/proc may be bare (holder-rooted elsewhere).
  // Args pass RAW (no translation): identity/aliasing/record-tags intact,
  // O(1) per call. Boxes in apply position are wrapped per the HOF table.
  Roots tmp;
  keep_in (tmp, sc, proc);
  std::vector<pointer> argv_mut= argv;
  hof_maybe_wrap (sc, proc, argv_mut);
  for (pointer a : argv_mut) keep_in (tmp, sc, a);
  if (s_raised_marker == nullptr)
    s_raised_marker= pin (sc, gf::make_symbol (sc, "gf0-raised"));
  pointer quote_sym= keep_in (tmp, sc, gf::make_symbol (sc, "quote"));
  std::vector<pointer> qargs;
  qargs.reserve (argv_mut.size ());
  for (pointer a : argv_mut) {
    qargs.push_back (keep_in (tmp, sc, gf::list (sc, quote_sym, a)));
  }
  pointer callexpr= keep_in (tmp, sc, gf::cons (sc, keep_in (tmp, sc, gf::list (sc, quote_sym, proc)),
                                       keep_in (tmp, sc, args_to_list (sc, qargs))));
  pointer thunk= keep_in (tmp, sc, gf::list (sc, keep_in (tmp, sc, gf::make_symbol (sc, "lambda")),
                                    gf::nil (sc), callexpr));
  // Catch s7 errors AT this boundary and convert to GfEx (a longjmp here
  // would fly past gf0 frames to the wrong catcher). The handler spreads
  // (MARKER . handler-args) as multiple values, so a raised call collects
  // with MARKER in head position, unambiguous against value shapes.
  pointer inner= keep_in (tmp, sc, gf::list (sc,
    keep_in (tmp, sc, gf::make_symbol (sc, "catch")),
    gf::t (sc),
    thunk,
    keep_in (tmp, sc, gf::list (sc,
      keep_in (tmp, sc, gf::make_symbol (sc, "lambda")),
      keep_in (tmp, sc, gf::make_symbol (sc, "hargs")),
      keep_in (tmp, sc, gf::list (sc,
        keep_in (tmp, sc, gf::make_symbol (sc, "apply")),
        keep_in (tmp, sc, gf::make_symbol (sc, "values")),
        keep_in (tmp, sc, gf::list (sc,
          keep_in (tmp, sc, gf::make_symbol (sc, "cons")),
          keep_in (tmp, sc, gf::list (sc, quote_sym, s_raised_marker)),
          keep_in (tmp, sc, gf::make_symbol (sc, "hargs"))))))))));
  pointer outer= keep_in (tmp, sc, gf::list (sc,
    keep_in (tmp, sc, gf::make_symbol (sc, "lambda")),
    gf::nil (sc), inner));
  pointer expr= keep_in (tmp, sc, gf::list (sc, keep_in (tmp, sc, gf::make_symbol (sc, "call-with-values")),
                                   outer, keep_in (tmp, sc, gf::make_symbol (sc, "list"))));
  pointer collected= gf::eval (sc, expr, gf::rootlet (sc));
  std::vector<pointer> out;
  pointer tail= collected;
  for (; gf::is_pair (tail); tail= gf::cdr (tail))
    out.push_back (keep_in (tmp, sc, gf::car (tail)));
  if (!gf::is_null (sc, tail))
    return multi_vec (sc, std::vector<pointer>{fail (sc, "gf0: improper collection", collected)});
  if (!out.empty () && out[0] == s_raised_marker) {
    GfEx e;
    for (size_t i= 1; i < out.size (); ++i) {
      e.args.push_back (out[i]);
      keep_in (e.roots, sc, out[i]);
    }
    throw e;
  }
  // s7 collapse rule: exactly 1 yielded value is single (sc, so (define x (+ 1 2))
  // stays single); 0 or 2+ stay multi. Matches the s7/guile oracle.
  if (out.size () == 1) return single (sc, out[0]);
  return multi_vec (sc, std::move (out));
}

static pointer
args_to_list (scheme* sc, const std::vector<pointer>& argv) {
  if (argv.empty ()) return gf::nil (sc);
  return gf::array_to_list (sc, (gf::int_) argv.size (),
                            const_cast<pointer*> (argv.data ()));
}

// Bind formals (proper / dotted / single symbol) to a value LIST,
// rooting into the target frame (dies with the frame's last holder).
static void
bind_formals (scheme* sc, pointer formals, pointer arglist,
              std::shared_ptr<Frame> fr, pointer ctx) {
  std::vector<Binding>& frame= fr->bindings;
  for (; gf::is_pair (formals); formals= gf::cdr (formals)) {
    if (!gf::is_pair (arglist))
      fail_key (sc, "wrong-number-of-args", "gf0: too few arguments", ctx);
    pointer name= gf::car (formals);
    if (!gf::is_symbol (name)) fail (sc, "gf0: non-symbol formal", name);
    V av;
    av.multi= false;
    av.one  = gf::car (arglist);
    frame_bind (sc, fr, name, av);
    arglist= gf::cdr (arglist);
  }
  if (gf::is_symbol (formals)) { // rest arg
    V av;
    av.multi= false;
    av.one  = arglist;
    frame_bind (sc, fr, formals, av);
  }
  else if (!gf::is_null (sc, formals)) {
    fail (sc, "gf0: improper formals", formals);
  }
  else if (gf::is_pair (arglist)) {
    fail_key (sc, "wrong-number-of-args", "gf0: too many arguments", ctx);
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
  bind_formals (sc, c.formals, arglist, inner.frames->back (), ctx);
  return inner;
}

// ---- CEK core (M-VM-2b): explicit control stack, capturable continuations.
// Control is an expr-to-run or yielded values; every nested position that
// used C++ recursion is now a Kont frame. Capturing = copying the Kont
// (all heap data; envs shared). s7call sites stay leaves.
enum class KK {
  Seq, If, CallP, CallA, Define, Set, LetB, RecB, ValsB, Vals,
  CwvP, CwvQ, CwvC, CatchG, CatchR, Mod, CcK, DwK, ProcP, ApplyP, ApplyA,
};
struct KF {
  KK tag;
  Env env;
  Env inner;
  pointer a;
  pointer b;
  pointer c;
  std::vector<pointer> acc;
  // Roots for acc/stored values; shared across Kont copies/captures.
  std::vector<Roots> roots;
  int stage;
  bool flag;
};
// Store any pointer (expr datum or value) into a Kont frame with roots.
static void
kstore (scheme* sc, KF& fr, pointer& slot, pointer p) {
  slot= p;
  Roots r= make_roots (sc);
  keep_in (r, sc, p);
  fr.roots.push_back (r);
}
// Pin a value for Kont-frame lifetime (acc elements etc.).
static void
kpin (scheme* sc, KF& fr, pointer p) {
  Roots r= make_roots (sc);
  keep_in (r, sc, p);
  fr.roots.push_back (r);
}
using Kont = std::vector<KF>;
struct Ctl {
  bool isVals;
  V v;
  pointer x;
  Env env;
};
static Ctl
ctlVals (V v) {
  Ctl c;
  c.isVals= true;
  c.v     = v;
  return c;
}
static Ctl
ctlExpr (pointer x, Env env) {
  Ctl c;
  c.isVals= false;
  c.x     = x;
  c.env   = env;
  return c;
}

// Dynamic-wind stack (M-VM-2b second half): session-global, copied on
// capture, spliced on invoke. depth = Kont size at push; a winder is due
// when unwinding reaches depth <= its own.
struct Winder {
  pointer before;
  pointer after;
  Env env;
  size_t depth;
};
struct Saved {
  Kont k;
  Env env;
  std::vector<Winder> winders;
};
static std::vector<Saved>      s_conts;
static std::unordered_set<void*> s_cont_boxes;
static gf::int_                s_cont_type= -1;
static std::vector<Winder> s_wind;

static Ctl stepE (scheme* sc, pointer x, Env env, Kont& k);
static Ctl plugInto (scheme* sc, KF fr, V v, Kont& k);
static V runLoop (scheme* sc, Ctl c, Kont& k);

// Bidirectional canonicalization (M3): gf0 boxes never cross into s7;
// s7 sees the memoized wrapper, and wrappers coming back unwrap to the
// box. Preserves eq?/assq/memq across the boundary (assoc-test).
static pointer wrap_box (scheme* sc, pointer box);

static pointer
wrap_box (scheme* sc, pointer box) {
  auto it= s_wrap_fwd.find ((void*) box);
  if (it != s_wrap_fwd.end ()) return it->second;
  pointer w= pin (sc, wrap_for_s7 (sc, box));
  s_wrap_fwd[(void*) box]= w;
  return w;
}

// s7 higher-order builtins that APPLY an argument: wrap gf0 boxes at the
// recorded positions so s7 never sees a raw box in apply position.
// Data positions pass RAW (no translation, no copies): identity, aliasing
// and record-type tags all preserved; O(1) per call (no quadratic walk).
// New HOFs surface as clean "attempt to apply" errors: add on demand.
static const struct HofEntry { const char* name; int p0; int p1; } kHofs[] = {
  {"map", 0, -1}, {"for-each", 0, -1}, {"filter", 0, -1},
  {"fold-left", 0, -1}, {"fold-right", 0, -1}, {"reduce", 0, -1},
  {"find", 0, -1}, {"find-tail", 0, -1}, {"any", 0, -1}, {"every", 0, -1},
  {"remove", 0, -1}, {"remp", 0, -1}, {"partition", 0, -1},
  {"sort", 1, -1}, {"sort!", 1, -1},
  {"string-map", 0, -1}, {"string-for-each", 0, -1}, {"string-fold", 0, -1},
  {"string-fold-right", 0, -1}, {"string-filter", 0, -1}, {"string-any", 0, -1},
  {"string-every", 0, -1}, {"string-unfold", 1, 2},
  {"vector-map", 0, -1}, {"vector-for-each", 0, -1}, {"vector-fold", 0, -1},
  {"vector-fold-right", 0, -1}, {"vector-filter", 0, -1}, {"vector-any", 0, -1},
  {"vector-every", 0, -1},
  {"with-exception-handler", 0, 1}, {"call-with-port", 0, -1},
  {"with-input-from-file", 1, -1}, {"with-output-to-file", 1, -1},
  {"call-with-input-file", 1, -1}, {"call-with-output-file", 1, -1},
  // Loader infrastructure applying callbacks internally (not user HOFs,
  // same rule: wrap so s7 never sees a raw box in apply position).
  {"register-runtime-module", 1, -1},
  {nullptr, -1, -1},
};
// Library-bound HOFs (not in rootlet: resolved via the substrate registry
// once their library is runtime-registered). Same rule, explicit (lib,name).
static const struct HofLib { const char* lib; const char* name; int p0; int p1; } kHofLibs[] = {
  {"(srfi srfi-133)", "vector-fold", 0, -1},
  {"(srfi srfi-133)", "vector-fold-right", 0, -1},
  {"(srfi srfi-133)", "vector-map!", 0, -1},
  {"(srfi srfi-133)", "vector-any", 0, -1},
  {"(srfi srfi-133)", "vector-every", 0, -1},
  {"(srfi srfi-133)", "vector-count", 0, -1},
  {"(srfi srfi-133)", "vector-index", 0, -1},
  {"(srfi srfi-133)", "vector-skip", 0, -1},
  {"(srfi srfi-133)", "vector-partition", 0, -1},
  {"(srfi srfi-78)", "check:proc", 1, -1},
  {"(srfi srfi-128)", "make-comparator", -2, -1},
  {"(srfi srfi-217)", "iset-search", 2, 3},
  {"(srfi srfi-217)", "iset-search!", 2, 3},
  {nullptr, nullptr, -1, -1},
};
static std::vector<std::pair<pointer, int>> s_hof_procs;
static bool s_hof_init= false;
static void
hof_maybe_wrap (scheme* sc, pointer proc, std::vector<pointer>& argv) {
  if (!s_hof_init) {
    s_hof_init= true;
    for (const HofEntry* e= kHofs; e->name != nullptr; ++e) {
      if (!gf::is_defined (sc, e->name)) continue;
      pointer p= pin (sc, gf::name_to_value (sc, e->name));
      s_hof_procs.push_back ({p, e->p0});
      if (e->p1 >= 0) s_hof_procs.push_back ({p, e->p1});
    }
    // Library HOFs: resolve through (module-ref 'lib 'name), guarded by
    // runtime-registered? so unloaded libs stay silent (no error-port noise).
    if (gf::is_defined (sc, "runtime-registered?") &&
        gf::is_defined (sc, "module-ref")) {
      pointer regq= gf::name_to_value (sc, "runtime-registered?");
      for (const HofLib* e= kHofLibs; e->lib != nullptr; ++e) {
        std::string libsrc= std::string ("'") + e->lib;
        pointer lib= gf::eval_c_string (sc, libsrc.c_str ());
        std::vector<pointer> one;
        one.push_back (lib);
        V r= s7call_vec (sc, regq, one);
        if (r.multi || !gf::boolean (sc, r.one)) continue;
        // Catch-wrapped: a missing export must yield #f, never longjmp out.
        std::string expr= std::string ("(catch #t (lambda () (module-ref '") +
                          e->lib + " '" + e->name + ")) (lambda args #f))";
        pointer p= gf::eval_c_string (sc, expr.c_str ());
        if (!gf::is_procedure (p)) continue;
        pin (sc, p);
        s_hof_procs.push_back ({p, e->p0});
        if (e->p1 >= 0) s_hof_procs.push_back ({p, e->p1});
      }
    }
  }
  for (const auto& h : s_hof_procs) {
    if (proc != h.first) continue;
    // Position -2 = constructor taking a callback bundle: wrap every box.
    if (h.second == -2) {
      for (size_t i= 0; i < argv.size (); ++i) {
        pointer a= argv[i];
        if (s_boxes.find ((void*) a) != s_boxes.end () ||
            s_cont_boxes.find ((void*) a) != s_cont_boxes.end ())
          argv[i]= wrap_box (sc, a);
      }
      continue;
    }
    if (h.second < 0 || (size_t) h.second >= argv.size ()) continue;
    pointer a= argv[(size_t) h.second];
    if (s_boxes.find ((void*) a) != s_boxes.end () ||
        s_cont_boxes.find ((void*) a) != s_cont_boxes.end ())
      argv[(size_t) h.second]= wrap_box (sc, a);
  }
}
static V callSync (scheme* sc, pointer proc);
static bool same_winder (const Winder& a, const Winder& b);

// Sequence control: empty -> unspecified value; single -> direct;
// longer -> first now, rest in a Seq frame.
static Ctl
seqCtl (scheme* sc, Env env, pointer body, Kont& k) {
  (void) sc;
  if (!gf::is_pair (body)) return ctlVals (single (sc, gf::unspecified (sc)));
  if (gf::is_null (sc, gf::cdr (body))) return ctlExpr (gf::car (body), env);
  KF fr;
  fr.tag= KK::Seq;
  fr.env= env;
  fr.a  = nullptr;
  fr.b  = gf::cdr (body);
  fr.c  = nullptr;
  fr.stage= 0;
  fr.flag= false;
  k.push_back (fr);
  return ctlExpr (gf::car (body), env);
}

static V runLoop (scheme* sc, Ctl c, Kont& k);
static V callSync (scheme* sc, pointer proc);
static bool same_winder (const Winder& a, const Winder& b);

// Apply a resolved proc to value vector -> Control (never nests C++ eval).
static Ctl
applyCtl (scheme* sc, pointer proc, const std::vector<pointer>& argvals, Kont& k) {
  if (s_boxes.find ((void*) proc) != s_boxes.end ()) {
    gf::int_ idx= gf::integer ((pointer) gf::c_object_value (proc));
    if (idx < 0 || (size_t) idx >= s_registry.size ())
      return ctlVals (single (sc, fail (sc, "gf0: stale closure", proc)));
    const Closure& c= s_registry[(size_t) idx];
    return seqCtl (sc, bind_call (sc, c, args_to_list (sc, argvals), proc), c.body, k);
  }
  if (s_cont_boxes.find ((void*) proc) != s_cont_boxes.end ()) {
    gf::int_ idx= gf::integer ((pointer) gf::c_object_value (proc));
    if (idx < 0 || (size_t) idx >= s_conts.size ())
      return ctlVals (single (sc, fail (sc, "gf0: bad continuation invoke", proc)));
    // R7RS: continuations take any number of values (s7/guile oracle:
    // zero args -> zero values, one -> single, n -> multi-n).
    // Splice winders: run afters of the abandoned suffix (innermost first),
    // adopt the target stack, run befores of the entered suffix.
    const std::vector<Winder>& tgt= s_conts[(size_t) idx].winders;
    size_t common= 0;
    while (common < s_wind.size () && common < tgt.size () &&
           same_winder (s_wind[common], tgt[common]))
      ++common;
    for (size_t i= s_wind.size (); i-- > common;)
      callSync (sc, s_wind[i].after);
    s_wind= tgt;
    for (size_t i= common; i < s_wind.size (); ++i)
      callSync (sc, s_wind[i].before);
    k= s_conts[(size_t) idx].k; // copy-on-invoke: stored stays pristine (multi-shot)
    if (argvals.size () == 1) return ctlVals (single (sc, argvals[0]));
    std::vector<pointer> many= argvals;
    return ctlVals (multi_vec (sc, std::move (many)));
  }
  if (gf::is_procedure (proc)) return ctlVals (s7call_vec (sc, proc, argvals));
  // Not applicable: delegate to s7's apply so the NATIVE error object
  // (key + info, e.g. syntax-error for (apply 1 ...)) surfaces and
  // check-catch matches exactly.
  pointer s7apply= gf::name_to_value (sc, "apply");
  std::vector<pointer> aargs;
  aargs.push_back (proc);
  for (pointer a : argvals) aargs.push_back (a);
  return ctlVals (s7call_vec (sc, s7apply, aargs));
}

static pointer
cont_box (scheme* sc, const Kont& k, Env env, const std::vector<Winder>& w) {
  Saved s;
  s.k  = k;
  s.env= env;
  s.winders= w;
  s_conts.push_back (s);
  gf::int_ idx= (gf::int_) (s_conts.size () - 1);
  pointer box= pin (sc, gf::make_c_object_with_let (
                      sc, c_type_cached (sc, "gf0-cont", &s_cont_type),
                      (void*) pin (sc, gf::make_integer (sc, idx)),
                      gf::rootlet (sc)));
  s_cont_boxes.insert ((void*) box);
  return box;
}

static bool
is_cc_name (pointer x) {
  return gf::is_symbol (x) &&
         (std::strcmp (gf::symbol_name (x), "call/cc") == 0 ||
          std::strcmp (gf::symbol_name (x), "call-with-current-continuation") == 0);
}

static Ctl
stepE (scheme* sc, pointer x, Env env, Kont& k) {
  if (stack_low ())
    return ctlVals (single (sc, fail (sc, "gf0: C stack low (non-tail depth; see M-VM)", x)));
  // Self-evaluating (keywords first: s7 keywords satisfy is_symbol but
  // evaluate to themselves).
  if (gf::is_boolean (x) || gf::is_number (x) || gf::is_string (x) ||
      gf::is_character (x) || gf::is_null (sc, x) || gf::is_vector (x) ||
      gf::is_keyword (x) || x == gf::eof_object (sc) ||
      x == gf::unspecified (sc) || x == gf::undefined (sc))
    return ctlVals (single (sc, x));
  if (gf::is_symbol (x)) {
    pointer v= lookup_raw (sc, x, env);
    if (v == unassigned_box (sc))
      return ctlVals (single (sc, fail (sc, "gf0: read before assignment (R7RS letrec)", x)));
    return ctlVals (single (sc, v));
  }
  if (!gf::is_pair (x)) return ctlVals (single (sc, fail (sc, "gf0: cannot evaluate", x)));

  if (is_head (x, "quote")) {
    if (!gf::is_pair (gf::cdr (x)) || gf::is_pair (gf::cdr (gf::cdr (x))))
      return ctlVals (single (sc, fail (sc, "gf0: bad quote", x)));
    return ctlVals (single (sc, gf::car (gf::cdr (x))));
  }
  if (is_head (x, "if")) {
    // NOTE: s7 nil is a live pointer, never nullptr; shape checks must use
    // is_pair/is_null, not pointer comparison.
    pointer rest1= gf::cdr (x);
    if (!gf::is_pair (rest1)) return ctlVals (single (sc, fail (sc, "gf0: bad if", x)));
    pointer rest2= gf::cdr (rest1);
    if (!gf::is_pair (rest2)) return ctlVals (single (sc, fail (sc, "gf0: bad if", x)));
    pointer rest3= gf::cdr (rest2);
    if (!(gf::is_null (sc, rest3) ||
          (gf::is_pair (rest3) && gf::is_null (sc, gf::cdr (rest3)))))
      return ctlVals (single (sc, fail (sc, "gf0: bad if", x)));
    KF fr;
    fr.tag= KK::If;
    fr.env= env;
    fr.a  = gf::car (rest2);
    fr.b  = gf::is_pair (rest3) ? gf::car (rest3) : nullptr;
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= gf::is_pair (rest3);
    k.push_back (fr);
    return ctlExpr (gf::car (rest1), env);
  }
  if (is_head (x, "begin")) {
    return seqCtl (sc, env, gf::cdr (x), k);
  }
  if (is_head (x, "lambda")) {
    if (!gf::is_pair (gf::cdr (x)))
      return ctlVals (single (sc, fail (sc, "gf0: bad lambda", x)));
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
    return ctlVals (single (sc, box));
  }
  if (is_head (x, "define")) {
    pointer rest= gf::cdr (x);
    bool ok= gf::is_pair (rest) && gf::is_symbol (gf::car (rest)) &&
             gf::is_pair (gf::cdr (rest)) &&
             gf::is_null (sc, gf::cdr (gf::cdr (rest)));
    if (!ok)
      return ctlVals (single (sc, fail (sc, "gf0: only (define name exp); sugar stays frontend", x)));
    KF fr;
    fr.tag= KK::Define;
    fr.env= env;
    fr.a  = gf::car (rest);
    fr.b  = nullptr;
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (gf::cdr (rest)), env);
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
        return ctlVals (single (sc, fail (sc, "gf0: bad module-set", x)));
      KF fr;
      fr.tag= KK::Mod;
      fr.env= env;
      fr.a  = nullptr;
      fr.b  = gf::car (gf::cdr (mr));
      fr.c  = gf::car (tail);
      fr.stage= 0;
      fr.flag= false;
      k.push_back (fr);
      return ctlExpr (gf::car (gf::cdr (mr)), env);
    }
    bool ok= gf::is_pair (rest) && gf::is_symbol (gf::car (rest)) &&
             gf::is_pair (gf::cdr (rest)) &&
             gf::is_null (sc, gf::cdr (gf::cdr (rest)));
    if (!ok) return ctlVals (single (sc, fail (sc, "gf0: bad set!", x)));
    KF fr;
    fr.tag= KK::Set;
    fr.env= env;
    fr.a  = gf::car (rest);
    fr.b  = nullptr;
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (gf::cdr (rest)), env);
  }
  if (is_head (x, "let") || is_head (x, "let*")) {
    bool star= (std::strcmp (gf::symbol_name (gf::car (x)), "let*") == 0);
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return ctlVals (single (sc, fail (sc, star ? "gf0: bad let*" : "gf0: bad let", x)));
    Env inner= push_frame (env);
    if (!gf::is_pair (gf::car (rest)))
      return seqCtl (sc, inner, gf::cdr (rest), k);
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return ctlVals (single (sc, fail (sc, star ? "gf0: bad let* binding" : "gf0: bad let binding", x)));
    }
    KF fr;
    fr.tag= KK::LetB;
    fr.env= env;
    fr.inner= inner;
    fr.a  = nullptr;
    fr.b  = gf::car (rest);
    fr.c  = gf::cdr (rest);
    fr.stage= 0;
    fr.flag= star;
    k.push_back (fr);
    return ctlExpr (gf::car (gf::cdr (gf::car (gf::car (rest)))), env);
  }
  if (is_head (x, "letrec") || is_head (x, "letrec*")) {
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return ctlVals (single (sc, fail (sc, "gf0: bad letrec", x)));
    Env inner= push_frame (env);
    std::shared_ptr<Frame> frame= inner.frames->back ();
    // Validate + pre-allocate all slots unassigned (R7RS: init-period reads
    // of any slot are an error; the reader reports the slot, not #<undefined>).
    for (pointer bs= gf::car (rest); gf::is_pair (bs); bs= gf::cdr (bs)) {
      pointer b= gf::car (bs);
      if (!gf::is_pair (b) || !gf::is_symbol (gf::car (b)) ||
          !gf::is_pair (gf::cdr (b)) || gf::is_pair (gf::cdr (gf::cdr (b))))
        return ctlVals (single (sc, fail (sc, "gf0: bad letrec binding", x)));
      V sv;
      sv.multi= false;
      sv.one  = unassigned_box (sc);
      frame_bind (sc, frame, gf::car (b), sv);
    }
    if (!gf::is_pair (gf::car (rest)))
      return seqCtl (sc, inner, gf::cdr (rest), k);
    KF fr;
    fr.tag= KK::RecB;
    fr.env= inner;
    fr.a  = gf::car (rest);
    fr.b  = gf::car (rest);
    fr.c  = gf::cdr (rest);
    fr.stage= 0;
    fr.flag= (std::strcmp (gf::symbol_name (gf::car (x)), "letrec*") == 0);
    k.push_back (fr);
    return ctlExpr (gf::car (gf::cdr (gf::car (gf::car (rest)))), inner);
  }
  if (is_head (x, "let-values")) {
    // Lowered code already has call-with-values; accept raw let-values.
    // Inits evaluate in the OUTER env (R7RS), then bind.
    pointer rest= gf::cdr (x);
    pointer binds= gf::is_pair (rest) ? gf::car (rest) : nullptr;
    if (binds == nullptr ||
        (!gf::is_null (sc, binds) && !gf::is_pair (binds)))
      return ctlVals (single (sc, fail (sc, "gf0: bad let-values", x)));
    Env inner= push_frame (env);
    if (!gf::is_pair (gf::car (rest)))
      return seqCtl (sc, inner, gf::cdr (rest), k);
    for (pointer cs= gf::car (rest); gf::is_pair (cs); cs= gf::cdr (cs)) {
      pointer c= gf::car (cs);
      if (!gf::is_pair (c) || !gf::is_pair (gf::cdr (c)) ||
          gf::is_pair (gf::cdr (gf::cdr (c))))
        return ctlVals (single (sc, fail (sc, "gf0: bad let-values clause", x)));
    }
    KF fr;
    fr.tag= KK::ValsB;
    fr.env= env;
    fr.inner= inner;
    fr.a  = nullptr;
    fr.b  = gf::car (rest);
    fr.c  = gf::cdr (rest);
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (gf::cdr (gf::car (gf::car (rest)))), env);
  }
  if (is_head (x, "values")) {
    pointer tail= gf::cdr (x);
    if (!gf::is_pair (tail)) {
      if (!gf::is_null (sc, tail))
        return ctlVals (single (sc, fail (sc, "gf0: improper values", x)));
      std::vector<pointer> empty;
      return ctlVals (multi_vec (sc, std::move (empty)));
    }
    KF fr;
    fr.tag= KK::Vals;
    fr.env= env;
    fr.a  = nullptr;
    fr.b  = gf::cdr (tail);
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (tail), env);
  }
  if (is_head (x, "catch")) {
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || !gf::is_pair (gf::cdr (rest)) ||
        !gf::is_pair (gf::cdr (gf::cdr (rest))) ||
        gf::is_pair (gf::cdr (gf::cdr (gf::cdr (rest)))))
      return ctlVals (single (sc, fail (sc, "gf0: bad catch", x)));
    KF fr;
    fr.tag= KK::CatchG;
    fr.env= env;
    fr.a  = nullptr;
    fr.b  = gf::car (gf::cdr (rest));
    fr.c  = gf::car (gf::cdr (gf::cdr (rest)));
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (rest), env);
  }
  if (is_head (x, "call-with-values")) {
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || !gf::is_pair (gf::cdr (rest)) ||
        gf::is_pair (gf::cdr (gf::cdr (rest))))
      return ctlVals (single (sc, fail (sc, "gf0: bad call-with-values", x)));
    KF fr;
    fr.tag= KK::CwvP;
    fr.env= env;
    fr.a  = nullptr;
    fr.b  = gf::car (gf::cdr (rest));
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (rest), env);
  }
  // Call: (proc args...).
  pointer head= gf::car (x);
  if (is_cc_name (head) && !user_bound (head, env)) {
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || gf::is_pair (gf::cdr (rest)))
      return ctlVals (single (sc, fail (sc, "gf0: call/cc takes exactly one proc", x)));
    KF fr;
    fr.tag= KK::CcK;
    fr.env= env;
    fr.a  = nullptr;
    fr.b  = nullptr;
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (rest), env);
  }
  if (gf::is_symbol (head) && std::strcmp (gf::symbol_name (head), "dynamic-wind") == 0 &&
      !user_bound (head, env)) {
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || !gf::is_pair (gf::cdr (rest)) ||
        !gf::is_pair (gf::cdr (gf::cdr (rest))) ||
        gf::is_pair (gf::cdr (gf::cdr (gf::cdr (rest)))))
      return ctlVals (single (sc, fail (sc, "gf0: bad dynamic-wind", x)));
    KF fr;
    fr.tag= KK::DwK;
    fr.env= env;
    fr.a  = nullptr;
    fr.b  = gf::car (gf::cdr (rest));
    fr.c  = gf::car (gf::cdr (gf::cdr (rest)));
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (rest), env);
  }
  if (gf::is_symbol (head) && std::strcmp (gf::symbol_name (head), "procedure?") == 0 &&
      !user_bound (head, env)) {
    // R7RS: closures and continuations satisfy procedure? (s7/guile agree).
    // s7's own predicate rejects c_object boxes, so answer natively.
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || gf::is_pair (gf::cdr (rest)))
      return ctlVals (single (sc, fail (sc, "gf0: bad procedure?", x)));
    KF fr;
    fr.tag= KK::ProcP;
    fr.env= env;
    fr.a  = nullptr;
    fr.b  = nullptr;
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (rest), env);
  }
  if (gf::is_symbol (head) && std::strcmp (gf::symbol_name (head), "apply") == 0 &&
      !user_bound (head, env)) {
    // R7RS apply with a gf0 proc: route natively (s7's apply rejects boxes).
    pointer rest= gf::cdr (x);
    if (!gf::is_pair (rest) || !gf::is_pair (gf::cdr (rest)))
      return ctlVals (single (sc, fail (sc, "gf0: bad apply", x)));
    KF fr;
    fr.tag= KK::ApplyP;
    fr.env= env;
    fr.a  = nullptr;
    fr.b  = gf::cdr (rest);
    fr.c  = nullptr;
    fr.stage= 0;
    fr.flag= false;
    k.push_back (fr);
    return ctlExpr (gf::car (rest), env);
  }
  if (is_frontend_syntax (head, env))
    return ctlVals (single (sc, fail (sc, "gf0: not core; desugar via frontend", head)));
  KF fr;
  fr.tag= KK::CallP;
  fr.env= env;
  fr.a  = nullptr;
  fr.b  = gf::cdr (x);
  fr.c  = nullptr;
  fr.stage= 0;
  fr.flag= false;
  k.push_back (fr);
  return ctlExpr (head, env);
}

static pointer
lookup_assign (scheme* sc, Env env, pointer name, V v, pointer ctx) {
  const char* want= gf::symbol_name (name);
  for (size_t i= env.frames->size (); i-- > 0;) {
    std::shared_ptr<Frame> fr= (*env.frames)[i];
    std::vector<Binding>& bs= fr->bindings;
    for (size_t j= bs.size (); j-- > 0;) {
      if (std::strcmp (gf::symbol_name (bs[j].sym), want) == 0) {
        bs[j].val= v.one;
        if (v.roots) fr->roots.push_back (v.roots);
        else { Roots r= make_roots (sc); keep_in (r, sc, v.one); fr->roots.push_back (r); }
        return nullptr;
      }
    }
  }
  // M2: assignment targets gf0 frames only; the session top frame is
  // seeded from the rootlet once (ensure_top). No live fallback.
  (void) sc;
  (void) ctx;
  fail_key (sc, "unbound-variable", "gf0: set! of unbound variable", name);
  return nullptr; // unreachable; keeps form
}

// s7 call protocol: argument value-lists concatenate, then arity checks
// (list/+/vector splice; closures get spread args; fixed-arity mismatch
// errors at apply). Splice multi into a Kont frame's acc.
static void
splice_into (scheme* sc, KF& fr, V& v) {
  if (v.multi) {
    for (pointer p : v.many) {
      fr.acc.push_back (p);
      kpin (sc, fr, p);
    }
  } else {
    fr.acc.push_back (v.one);
    kpin (sc, fr, v.one);
  }
}

static Ctl
plugInto (scheme* sc, KF fr, V v, Kont& k) {
  switch (fr.tag) {
  case KK::Seq: {
    return seqCtl (sc, fr.env, fr.b, k);
  }
  case KK::If: {
    // s7 tests the FIRST value (zero values count as true).
    pointer t= v.multi ? (v.many.empty () ? gf::t (sc) : v.many[0]) : v.one;
    if (!gf::boolean (sc, t))
      return fr.flag ? ctlExpr (fr.b, fr.env)
                     : ctlVals (single (sc, gf::unspecified (sc)));
    return ctlExpr (fr.a, fr.env);
  }
  case KK::CallP: {
    V proc_v = must_single (sc, v, fr.b, "gf0: call head must be single-valued"); pointer proc = proc_v.one;
    if (s_boxes.find ((void*) proc) == s_boxes.end () &&
        s_cont_boxes.find ((void*) proc) == s_cont_boxes.end () &&
        !gf::is_procedure (proc))
      return ctlVals (single (sc, fail (sc, "gf0: cannot apply (macros stay frontend)", proc)));
    if (!gf::is_pair (fr.b))
      return applyCtl (sc, proc, std::vector<pointer> (), k);
    KF na;
    na.tag= KK::CallA;
    na.env= fr.env;
    na.a  = proc; kpin (sc, na, proc);
    na.b  = gf::cdr (fr.b);
    na.c  = nullptr;
    na.stage= 0;
    na.flag= false;
    k.push_back (na);
    return ctlExpr (gf::car (fr.b), fr.env);
  }
  case KK::CallA: {
    splice_into (sc, fr, v);
    if (gf::is_pair (fr.b)) {
      pointer next= gf::car (fr.b);
      fr.b= gf::cdr (fr.b);
      k.push_back (fr);
      return ctlExpr (next, fr.env);
    }
    return applyCtl (sc, fr.a, fr.acc, k);
  }
  case KK::Define: {
    V init_v = must_single (sc, v, fr.a, "gf0: define init must be single-valued"); pointer init = init_v.one;
    (void) init;
    frame_bind (sc, fr.env.frames->back (), fr.a, init_v);
    return ctlVals (single (sc, gf::unspecified (sc)));
  }
  case KK::Set: {
    V val_v = must_single (sc, v, fr.a, "gf0: set! value must be single-valued"); pointer val = val_v.one;
    (void) val;
    lookup_assign (sc, fr.env, fr.a, val_v, fr.a);
    return ctlVals (single (sc, gf::unspecified (sc)));
  }
  case KK::LetB: {
    pointer b= gf::car (fr.b);
    V init_v = must_single (sc, v, b, "gf0: let init must be single-valued"); pointer init = init_v.one;
    (void) init;
    frame_bind (sc, fr.inner.frames->back (), gf::car (b), init_v);
    if (gf::is_pair (gf::cdr (fr.b))) {
      pointer nb= gf::car (gf::cdr (fr.b));
      fr.b= gf::cdr (fr.b);
      k.push_back (fr);
      return ctlExpr (gf::car (gf::cdr (nb)),
                      fr.flag ? fr.inner : fr.env);
    }
    return seqCtl (sc, fr.inner, fr.c, k);
  }
  case KK::RecB: {
    pointer b= gf::car (fr.b);
    V init_v = must_single (sc, v, b, "gf0: letrec init must be single-valued"); pointer init = init_v.one;
    (void) init;
    if (fr.flag) {
      // letrec*: sequential, later inits see earlier bindings.
      lookup_assign (sc, fr.env, gf::car (b), init_v, b);
    } else {
      // letrec: isolate; every init sees all slots unassigned (reads
      // error), assignment happens once all inits are collected.
      fr.acc.push_back (init_v.one);
      kpin (sc, fr, init_v.one);
    }
    if (gf::is_pair (gf::cdr (fr.b))) {
      pointer nb= gf::car (gf::cdr (fr.b));
      fr.b= gf::cdr (fr.b);
      k.push_back (fr);
      return ctlExpr (gf::car (gf::cdr (nb)), fr.env);
    }
    if (!fr.flag) {
      pointer bs= fr.a;
      for (size_t i= 0; gf::is_pair (bs); bs= gf::cdr (bs), ++i)
        lookup_assign (sc, fr.env, gf::car (gf::car (bs)), single (sc, fr.acc[i]), b);
    }
    return seqCtl (sc, fr.env, fr.c, k);
  }
  case KK::ValsB: {
    pointer c= gf::car (fr.b);
    V iv= v;
    std::vector<pointer> many;
    if (iv.multi) many= iv.many;
    else many.push_back (iv.one);
    bind_formals (sc, gf::car (c), args_to_list (sc, many),
                  fr.inner.frames->back (), c);
    if (gf::is_pair (gf::cdr (fr.b))) {
      pointer nc= gf::car (gf::cdr (fr.b));
      fr.b= gf::cdr (fr.b);
      k.push_back (fr);
      return ctlExpr (gf::car (gf::cdr (nc)), fr.env);
    }
    return seqCtl (sc, fr.inner, fr.c, k);
  }
  case KK::Vals: {
    splice_into (sc, fr, v);
    if (gf::is_pair (fr.b)) {
      pointer next= gf::car (fr.b);
      fr.b= gf::cdr (fr.b);
      k.push_back (fr);
      return ctlExpr (next, fr.env);
    }
    return ctlVals (multi_vec (sc, std::move (fr.acc)));
  }
  case KK::CwvP: {
    V producer_v = must_single (sc, v, fr.b, "gf0: cwv producer must be single-valued"); pointer producer = producer_v.one;
    KF nq;
    nq.tag= KK::CwvQ;
    nq.env= fr.env;
    nq.a  = nullptr;
    nq.b  = fr.b;
    nq.c  = nullptr;
    nq.stage= 0;
    nq.flag= false;
    k.push_back (nq);
    // R7RS: the producer is CALLED with zero args; its values feed consumer.
    return applyCtl (sc, producer, std::vector<pointer> (), k);
  }
  case KK::CwvQ: {
    std::vector<pointer> got;
    if (v.multi) got= v.many;
    else got.push_back (v.one);
    KF nc;
    nc.tag= KK::CwvC;
    nc.env= fr.env;
    nc.a  = nullptr;
    nc.b  = nullptr;
    nc.c  = nullptr;
    nc.acc= std::move (got);
    nc.stage= 0;
    nc.flag= false;
    k.push_back (nc);
    return ctlExpr (fr.b, fr.env);
  }
  case KK::CwvC: {
    V consumer_v = must_single (sc, v, gf::nil (sc), "gf0: cwv consumer must be single-valued"); pointer consumer = consumer_v.one;
    return applyCtl (sc, consumer, fr.acc, k);
  }
  case KK::CatchG: {
    if (fr.stage == 0) {
      V tag_v = must_single (sc, v, fr.b, "gf0: catch tag must be single-valued"); pointer tag = tag_v.one;
      fr.a= tag; kpin (sc, fr, tag);
      fr.stage= 1;
      k.push_back (fr);
      return ctlExpr (fr.b, fr.env);
    }
    if (fr.stage == 1) {
      V thunk_v = must_single (sc, v, fr.b, "gf0: catch thunk must be single-valued"); pointer thunk = thunk_v.one;
      fr.acc.push_back (thunk); kpin (sc, fr, thunk);
      fr.stage= 2;
      k.push_back (fr);
      return ctlExpr (fr.c, fr.env);
    }
    V handler_v = must_single (sc, v, fr.b, "gf0: catch handler must be single-valued"); pointer handler = handler_v.one;
    KF nr;
    nr.tag= KK::CatchR;
    nr.env= fr.env;
    nr.a  = fr.a;
    kstore (sc, nr, nr.b, handler);
    nr.c  = nullptr;
    nr.stage= 0;
    nr.flag= false;
    k.push_back (nr);
    return applyCtl (sc, fr.acc[0], std::vector<pointer> (), k);
  }
  case KK::CatchR: {
    // Thunk completed without raising: drop the handler, pass the value on.
    return ctlVals (v);
  }
  case KK::Mod: {
    if (fr.stage == 0) {
      V lib_v = must_single (sc, v, fr.b, "gf0: module lib must be single-valued"); pointer lib = lib_v.one;
      fr.a= lib; kpin (sc, fr, lib);
      fr.stage= 1;
      k.push_back (fr);
      return ctlExpr (fr.b, fr.env);
    }
    if (fr.stage == 1) {
      V name_v = must_single (sc, v, fr.b, "gf0: module name must be single-valued"); pointer name = name_v.one;
      kstore (sc, fr, fr.b, name);
      fr.stage= 2;
      k.push_back (fr);
      return ctlExpr (fr.c, fr.env);
    }
    V val_v = must_single (sc, v, fr.b, "gf0: module-set value must be single-valued"); pointer val = val_v.one;
    pointer mr_proc= lookup_raw (sc, pin (sc, gf::make_symbol (sc, "module-ref")), fr.env);
    pointer setter= lookup_raw (sc, pin (sc, gf::make_symbol (sc, "setter")), fr.env);
    if (mr_proc == unassigned_box (sc) || !gf::is_procedure (mr_proc) ||
        setter == unassigned_box (sc) || !gf::is_procedure (setter))
      return ctlVals (single (sc, fail (sc, "gf0: module-ref unavailable (boot substrate?)", fr.b)));
    std::vector<pointer> one;
    one.push_back (mr_proc);
    V writer_v= must_single (sc, s7call_vec (sc, setter, one), fr.b,
                                 "gf0: (setter module-ref) must be single");
    pointer writer= writer_v.one;
    // NOTE: writer applied via s7call (leaf, completes here); nested user
    // code cannot intervene, so flatness holds.
    std::vector<pointer> args3;
    args3.push_back (fr.a);
    args3.push_back (fr.b);
    args3.push_back (pin (sc, val));
    must_single (sc, s7call_vec (sc, writer, args3), fr.b,
                 "gf0: module write must be single-valued");
    return ctlVals (single (sc, gf::unspecified (sc)));
  }
  case KK::CcK: {
    V proc_v = must_single (sc, v, gf::nil (sc), "gf0: call/cc arg must be single-valued"); pointer proc = proc_v.one;
    Saved s;
    s.k  = k;
    s.env= fr.env;
    s.winders= s_wind;
    pointer box= cont_box (sc, s.k, s.env, s.winders);
    std::vector<pointer> one;
    one.push_back (box);
    return applyCtl (sc, proc, one, k);
  }
  case KK::ProcP: {
    V arg_v = must_single (sc, v, gf::nil (sc), "gf0: procedure? arg must be single-valued"); pointer arg = arg_v.one;
    if (s_boxes.find ((void*) arg) != s_boxes.end () ||
        s_cont_boxes.find ((void*) arg) != s_cont_boxes.end ())
      return ctlVals (single (sc, gf::t (sc)));
    std::vector<pointer> one;
    one.push_back (pin (sc, arg));
    return ctlVals (s7call_vec (sc, lookup_raw (sc, pin (sc, gf::make_symbol (sc, "procedure?")),
                                                fr.env),
                                one));
  }
  case KK::ApplyP: {
    V proc_v = must_single (sc, v, fr.b, "gf0: apply head must be single-valued"); pointer proc = proc_v.one;
    if (!gf::is_pair (fr.b))
      return ctlVals (single (sc, fail (sc, "gf0: apply needs args", fr.b)));
    KF na;
    na.tag= KK::ApplyA;
    na.env= fr.env;
    na.a  = proc; kpin (sc, na, proc);
    na.b  = gf::cdr (fr.b);
    na.c  = nullptr;
    na.stage= 0;
    na.flag= false;
    k.push_back (na);
    return ctlExpr (gf::car (fr.b), fr.env);
  }
  case KK::ApplyA: {
    splice_into (sc, fr, v);
    if (gf::is_pair (fr.b)) {
      pointer next= gf::car (fr.b);
      fr.b= gf::cdr (fr.b);
      k.push_back (fr);
      return ctlExpr (next, fr.env);
    }
    // Last arg must be a proper list; spread prefix + elements.
    std::vector<pointer> combined;
    for (size_t i= 0; i + 1 < fr.acc.size (); ++i) combined.push_back (fr.acc[i]);
    pointer tail= fr.acc.back ();
    for (; gf::is_pair (tail); tail= gf::cdr (tail))
      combined.push_back (gf::car (tail));
    if (!gf::is_null (sc, tail))
      return ctlVals (single (sc, fail (sc, "gf0: apply last arg must be a list", tail)));
    return applyCtl (sc, fr.a, combined, k);
  }
  case KK::DwK: {
    // stages: 0 before-expr -> 1 thunk-expr -> 2 after-expr -> 3 thunk run
    // -> 4 after run (keeps thunk value). Winder pushed once after-proc is
    // known, before the thunk runs; popped on normal completion.
    if (fr.stage == 0) {
      V before_v = must_single (sc, v, fr.b, "gf0: dynamic-wind before must be single-valued"); pointer before = before_v.one;
      fr.a= before; kpin (sc, fr, before);
      fr.stage= 1;
      k.push_back (fr);
      return applyCtl (sc, before, std::vector<pointer> (), k);
    }
    if (fr.stage == 1) {
      fr.stage= 2;
      k.push_back (fr);
      return ctlExpr (fr.c, fr.env);
    }
    if (fr.stage == 2) {
      V after_v = must_single (sc, v, fr.c, "gf0: dynamic-wind after must be single-valued"); pointer after = after_v.one;
      kstore (sc, fr, fr.c, after);
      fr.stage= 3;
      k.push_back (fr);
      Winder wd;
      wd.before= fr.a;
      wd.after= after;
      wd.env  = fr.env;
      wd.depth= k.size ();
      s_wind.push_back (wd);
      return ctlExpr (fr.b, fr.env);
    }
    if (fr.stage == 3) {
      V thunkproc_v = must_single (sc, v, fr.b, "gf0: dynamic-wind thunk must be single-valued"); pointer thunkproc = thunkproc_v.one;
      fr.acc.push_back (thunkproc); kpin (sc, fr, thunkproc);
      fr.stage= 4;
      k.push_back (fr);
      return applyCtl (sc, thunkproc, std::vector<pointer> (), k);
    }
    if (fr.stage == 4) {
      // Thunk completed normally with value v: stash it, pop our winder,
      // run after, then return the stashed thunk value (stage 5).
      V thunkproc_v= must_single (sc, v, fr.b, "gf0: dynamic-wind thunk must be single-valued");
      fr.acc.push_back (thunkproc_v.one); kpin (sc, fr, thunkproc_v.one);
      if (!s_wind.empty ()) s_wind.pop_back ();
      fr.stage= 5;
      k.push_back (fr);
      return applyCtl (sc, fr.c, std::vector<pointer> (), k);
    }
    // stage 5: after ran (value ignored); thunk value is acc[1].
    return ctlVals (single (sc, fr.acc[1]));
  }
  return ctlVals (single (sc, fail (sc, "gf0: bad kont", fr.a)));
}
}

static V runLoop (scheme* sc, Ctl c, Kont& k);

// Synchronous proc call for winder bodies (before/after run to completion
// during a transfer). Nested driveLoop depth is bounded by winder nesting,
// not user recursion.
static V
callSync (scheme* sc, pointer proc) {
  Kont k2;
  return runLoop (sc, applyCtl (sc, proc, std::vector<pointer> (), k2), k2);
}

static bool
same_winder (const Winder& a, const Winder& b) {
  return a.before == b.before && a.after == b.after && a.env.frames == b.env.frames;
}

static V
runLoop (scheme* sc, Ctl c, Kont& k) {
  for (;;) {
    if (stack_low ())
      fail (sc, "gf0: C stack low (non-tail depth; see M-VM)", gf::nil (sc));
    try {
      if (c.isVals) {
        if (k.empty ()) return c.v;
        KF fr= k.back ();
        k.pop_back ();
        c= plugInto (sc, fr, c.v, k);
      }
      else {
        c= stepE (sc, c.x, c.env, k);
      }
    }
    catch (GfEx& e) {
      // Unwind one frame at a time, running due afters innermost-first.
      // A winder is due once unwinding reaches its push depth.
      for (;;) {
        while (!s_wind.empty () && s_wind.back ().depth >= k.size ()) {
          Winder wd= s_wind.back ();
          s_wind.pop_back ();
          callSync (sc, wd.after);
        }
        if (k.empty ()) throw;
        KF fr= k.back ();
        k.pop_back ();
        if (fr.tag != KK::CatchR) continue;
        bool all= gf::is_boolean (fr.a) && gf::boolean (sc, fr.a);
        if (!all && (e.args.empty () || !gf::is_eq (fr.a, e.args[0]))) continue;
        c= applyCtl (sc, fr.b, e.args, k);
        break;
      }
    }
  }
}

static V
run (scheme* sc, pointer x, Env env) {
  Kont k;
  return runLoop (sc, ctlExpr (x, env), k);
}

static pointer gfex_to_error (scheme* sc, GfEx& e);

// SPIKE: s7-side application of gf0 boxes (ref protocol). Convention per
// s7.h: ref receives the full combination (obj . args); args here arrive
// EVALUATED or not depending on path -- handled by probing both.
static gf::pointer
gf0_box_ref (gf::scheme* sc, gf::pointer args) {
  if (!gf::is_pair (args)) return gf::nil (sc);
  pointer box= gf::car (args);
  std::vector<pointer> argvals;
  for (pointer t= gf::cdr (args); gf::is_pair (t); t= gf::cdr (t))
    argvals.push_back (gf::car (t));
  try {
    Kont k;
    V r= runLoop (sc, applyCtl (sc, box, argvals, k), k);
    if (!r.multi) return r.one;
    return gf::values (sc, args_to_list (sc, r.many));
  }
  catch (GfEx& e) {
    return gfex_to_error (sc, e);
  }
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

// Copy an inlet's (name . value) cells into the session top frame by
// reference (pinned). Used once for the rootlet (M2 snapshot) and on
// demand for library inlets (M2a artifact bridge: gensyms resolve only
// in the-expander-library).
static void
seed_from_inlet (scheme* sc, pointer inlet) {
  pointer alist= gf::let_to_list (sc, inlet);
  for (; gf::is_pair (alist); alist= gf::cdr (alist)) {
    pointer e= gf::car (alist);
    if (gf::is_pair (e) && gf::is_symbol (gf::car (e))) {
      pointer sym= gf::car (e);
      pointer tail= gf::cdr (e);
      pointer val= gf::is_pair (tail) ? gf::car (tail) : tail;
      V sv;
      sv.multi= false;
      sv.one  = val;
      frame_bind (sc, s_top.frames->back (), sym, sv);
    }
  }
}

static void
ensure_top (scheme* sc) {
  stack_init_once ();
  if (s_top.frames == nullptr) {
    s_top.frames= std::make_shared<std::vector<std::shared_ptr<Frame>>> ();
    // Frame 0 = host snapshot (M2 seed, never user code); frame 1+ = user.
    // Shadow checks (user_bound) skip frame 0 so seeded host names never
    // count as user shadowing; lookup/set! see all frames normally.
    s_top.frames->push_back (std::make_shared<Frame> ());
    // M2 snapshot: own every toplevel cell from here on. Primitives stay
    // host values (referenced, never re-resolved); later s7-side rootlet
    // definitions are invisible by design.
    seed_from_inlet (sc, gf::rootlet (sc));
    s_top.frames->push_back (std::make_shared<Frame> ());
  }
}

static gf::pointer
f_gf0_apply (scheme* sc, pointer args) {
  pointer box= gf::car (args);
  pointer tail= gf::cdr (args);
  if (!gf::is_pair (tail) || gf::is_pair (gf::cdr (tail)))
    return fail (sc, "gf0: g_gf0-apply takes (box arglist)", args);
  pointer arglist= gf::car (tail);
  if (s_boxes.find ((void*) box) == s_boxes.end () &&
      s_cont_boxes.find ((void*) box) == s_cont_boxes.end ())
    return fail (sc, "gf0: stale closure", box);
  try {
    std::vector<pointer> argvals;
    for (pointer t= arglist; gf::is_pair (t); t= gf::cdr (t))
      argvals.push_back (gf::car (t));
    if (!gf::is_null (sc, arglist) && argvals.empty ())
      return fail (sc, "gf0: improper arglist", box);
    // NOTE: improper non-empty tails surface in bind_formals; keep flat.
    Kont k;
    V r= runLoop (sc, applyCtl (sc, box, argvals, k), k);
    // Trampoline returns feed S7 consumers: keep s7-canonical (wrapped)
    // so host identity comparisons (equal?/assq) hold on that side.
    V one_v= must_single (sc, r, box, "gf0: s7 callback must be single-valued");
    return one_v.one;
  }
  catch (GfEx& e) {
    return gfex_to_error (sc, e);
  }
}

static gf::pointer
f_gf0_apply_values (scheme* sc, pointer args) {
  pointer box= gf::car (args);
  pointer tail= gf::cdr (args);
  if (!gf::is_pair (tail) || gf::is_pair (gf::cdr (tail)))
    return fail (sc, "gf0: g_gf0-apply-values takes (box arglist)", args);
  pointer arglist= gf::car (tail);
  if (s_boxes.find ((void*) box) == s_boxes.end () &&
      s_cont_boxes.find ((void*) box) == s_cont_boxes.end ())
    return fail (sc, "gf0: stale closure", box);
  try {
    std::vector<pointer> argvals;
    for (pointer t= arglist; gf::is_pair (t); t= gf::cdr (t))
      argvals.push_back (gf::car (t));
    Kont k;
    V r= runLoop (sc, applyCtl (sc, box, argvals, k), k);
    // Trampoline returns feed S7 consumers: raw (s7-canonical identity;
    // wrappers only where s7 must apply, per the HOF table).
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

static gf::pointer
f_gf0_eval (scheme* sc, pointer args) {
  ensure_top (sc);
  try {
    V r= run (sc, gf::car (args), s_top);
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

static gf::pointer
f_gf0_s7call_token (scheme* sc, pointer args) {
  (void) args;
  if (s_s7call_stack.empty ()) return gf::f (sc);
  return gf::make_integer (sc, s_s7call_stack.back ());
}

// Seed the session top env from an s7 inlet (e.g. the-expander-library):
// compiled artifacts reference library bindings by gensym, which only
// resolve there. Copies (name . value) cells by reference (pinned); gf0
// set! writes its own frames, never back into the inlet. M2a differential
// bridge; toplevel cells (M2) retire it.
//
// Side effect: installs the s7-side stale-continuation fence at the rootlet
// (all library lookup chains end there): call/cc and
// call-with-current-continuation are shadowed by serial-guarded wrappers.
// Same-crossing use is unaffected; cross-boundary invoke raises
// gf0-stale-continuation. gf0's own call/cc stays native (name-intercepted
// in stepE, never resolved through the inlet). Idempotent across re-imports.
static const char kStaleCcGuard[] =
  "(eval '(begin "
  "  (unless (defined? '%gf0-native-call/cc) "
  "    (define %gf0-native-call/cc call/cc) "
  "    (define %gf0-native-call-with-current-continuation call-with-current-continuation) "
  "    (define (%gf0-guarded-cc native proc) "
  "      (let ((tok (g_gf0-s7call-token))) "
  "        (native (lambda (k) "
  "                  (proc (lambda args "
  "                          (if (equal? tok (g_gf0-s7call-token)) "
  "                              (apply k args) "
  "                              (error 'gf0-stale-continuation "
  "                                     \"s7 continuation invoked after its s7call frame returned\"))))))))) "
  "  (define (call/cc proc) (%gf0-guarded-cc %gf0-native-call/cc proc)) "
  "  (define (call-with-current-continuation proc) "
  "    (%gf0-guarded-cc %gf0-native-call-with-current-continuation proc)) "
  "  (if #f #f)) "
  "  (rootlet))";
static gf::pointer
f_gf0_import_inlet (scheme* sc, pointer args) {
  // Optional second arg #f = fence only: install the stale-cc shadow without
  // touching gf0 state (no ensure_top, so the frame-0 rootlet snapshot is NOT
  // taken yet). Load libraries AFTER this call so their call/cc wiring bakes
  // guarded; then call with one arg to seed (snapshot now covers the libs).
  bool fence_only= gf::is_pair (gf::cdr (args)) &&
                   !gf::boolean (sc, gf::car (gf::cdr (args)));
  if (!fence_only) ensure_top (sc);
  if (!fence_only) seed_from_inlet (sc, gf::car (args));
  gf::eval_c_string (sc, kStaleCcGuard);
  return gf::unspecified (sc);
}

static gf::pointer
f_gf0_eval_values (scheme* sc, pointer args) {
  ensure_top (sc);
  try {
    V r= run (sc, gf::car (args), s_top);
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
  gf::define_function (sc, "g_gf0-import-inlet", gf0::f_gf0_import_inlet, 1, 1, false,
                        "(g_gf0-import-inlet inlet [seed?]) => unspecified. Seed gf0 session env from an s7 inlet; "
                        "with #f as second arg, install only the stale-cc fence (no seed, no snapshot yet): "
                        "call fenced BEFORE loading libraries, then seed after so the snapshot covers them");
  gf::define_function (sc, "g_gf0-apply", gf0::f_gf0_apply, 2, 0, false,
                       "(g_gf0-apply box arglist) => value, apply a gf0 closure box (s7 callback entry)");
  gf::define_function (sc, "g_gf0-apply-values", gf0::f_gf0_apply_values, 2, 0, false,
                        "(g_gf0-apply-values box arglist) => list of values from a gf0 closure box");
  gf::define_function (sc, "g_gf0-s7call-token", gf0::f_gf0_s7call_token, 0, 0, false,
                        "(g_gf0-s7call-token) => innermost live s7call token, #f outside (stale-cc fence)");
}

} // namespace goldfish
