//
// goldfish_vm.cpp -- the self-hosted bytecode VM.
//
// Executes the bytecode emitted by (goldfish compiler bytecode), in the
// positional form produced by encode-bytecode:
//   (program (code-table (code nlocals formals #(op payload i0 i1 ...)) ...)
//            (top nlocals #(op payload i0 i1 ...)))
// Opcode numbers pair with vm-opcodes in bytecode.scm (unstable
// pre-release, frozen at the first release).
//
// A VM function is an s7 closure shell `(lambda formals (vm-enter
// <cobj> formals...))` whose c_object (type VM_CLOSURE) carries
// {code index, captured frames}.  Lexical addressing: a frame's slots
// live in an s7 vector (shared by closures, so set! on captured
// variables is visible), and the captured chain is a list of the
// enclosing frames' slot vectors (outermost first).  (ref d i) /
// (set-ref d i) index that chain, (local i) / (set-local i) the current
// frame, (global name) / (store-global name) the global environment.
//
// Value passing: one global value stack, each frame owns the region
// [stack_base, g_current_vm->stack.size()).  When a frame ends (a (return) or
// running off the end of its instruction list) its top-of-region value
// is popped and handed to the enclosing frame's region -- or returned by
// run() if no enclosing frame remains below the target depth.  A
// non-tail call to a VM closure pushes a new frame whose region starts
// at the current stack top; a tail call to a VM closure replaces the
// current frame in place (no frame churn, no dangling references).
//
// The instruction stream is pre-decoded at load time into C++ structs
// (enum opcode + fixed operands, jump targets resolved to indices), so
// the interpreter loop is a plain switch -- no s7 list walking at run
// time.  GC is disabled while the VM runs (its stacks hold gf::pointer
// values in C++ containers the conservative GC cannot see); captured slot
// vectors live in the c_object's let, which the GC marks, so closures
// survive across runs.  The loaded program datum is gc-protected.

#include "gf.h"
#include <deque>
#include <string>
#include <unordered_map>
#include <vector>

namespace goldfish {

static gf::int_ VM_CLOSURE_TYPE = 0;

// ---------------------------------------------------------------------------
// Decoded instruction.

// Opcode numbers must match the vm-opcodes table in
// goldfish/compiler/bytecode.scm (encode-bytecode).  Pre-release the
// numbering is unstable: renumber freely, both sides together; it
// freezes into an ABI at the first release.  Multiple values are a
// derived form (list representation, see base-functions.scm): the VM
// is single-value.
enum class Op : uint8_t {
  Const, Global, Ref, Local, SetLocal, SetRef, StoreGlobal,
  Closure, Call, TailCall, IfElse, Jump, Return, Pop, Unknown
};

struct Instr {
  Op op = Op::Unknown;
  gf::pointer a = nullptr;  // symbol/constant operand
  gf::int_ b = 0;            // integer operand (depth/arity/label)
  gf::int_ c = 0;            // second integer (slot index for ref/set-ref)
};

static gf::pointer vm_enter_symbol  = nullptr;  // 'vm-enter
static gf::pointer quote_symbol     = nullptr;  // 'quote
static gf::pointer captured_symbol  = nullptr;  // '*vm-captured*
static gf::pointer g_false = nullptr;           // cached #f (unique object)

// ---------------------------------------------------------------------------
// Instruction decoding.

// The program carries pre-encoded flat vectors (encode-bytecode in
// goldfish/compiler/bytecode.scm): four slots per instruction -- opcode,
// payload, i0, i1 -- with labels already resolved to instruction
// indices.  Decoding just lays the slots out into Instr records.
static std::vector<Instr> decode_instrs (gf::scheme* sc, gf::pointer vec) {
  std::vector<Instr> v;
  if (!gf::is_vector (vec)) return v;
  gf::int_ n = gf::vector_length (vec);
  for (gf::int_ i = 0; i + 3 < n; i += 4) {
    Instr in;
    gf::int_ opn = gf::integer (gf::vector_ref (sc, vec, i));
    in.op = (opn >= 0 && opn < (gf::int_)Op::Unknown) ? (Op)opn : Op::Unknown;
    in.a  = gf::vector_ref (sc, vec, i + 1);
    in.b  = gf::integer (gf::vector_ref (sc, vec, i + 2));
    in.c  = gf::integer (gf::vector_ref (sc, vec, i + 3));
    v.push_back (in);
  }
  return v;
}

struct VMProgram;
struct VMClosure {
  VMProgram* prog;     // the program this closure's code lives in
  int code_idx;
  gf::pointer captured;  // list of enclosing slot vectors (outermost first)
  gf::pointer global_env;  // the (global ...) resolution env of this program
};

struct VMCodeInfo {
  int nlocals;
  gf::pointer formals;
  std::vector<Instr> instrs;  // pre-decoded
};

struct VMProgram {
  std::vector<VMCodeInfo> codes;
  std::vector<Instr> top;
};

struct VMFrame {
  size_t pc = 0;
  const std::vector<Instr>* code = nullptr;
  VMProgram* prog = nullptr;  // the program this frame executes (not the
                              // global g_current_vm->prog, which a nested vm-load can
                              // replace mid-execution)
  gf::pointer slots = nullptr;  // frame slots: an s7 vector (the SINGLE
                               // storage -- Local/SetLocal and closure
                               // capture (Ref/SetRef) alias this same
                               // vector, so set! on a captured variable is
                               // visible both sides with no sync)
  gf::pointer captured = nullptr;  // list of enclosing slot vectors
                                  // (outermost first)
  gf::pointer global_env = nullptr;  // (global ...) resolution env of this frame
  size_t stack_base = 0;  // g_current_vm->stack index delimiting this frame's value
                          // region; the frame owns [stack_base, g_current_vm->stack.size())
};

struct VM {
  VMProgram* prog = nullptr;
  std::vector<gf::pointer> stack;
  std::deque<VMFrame> frames;
};
static VM g_vm;
static std::unordered_map<VMProgram*, VM*> g_prog_map;
static std::vector<VM*> g_vm_stack;
static VM* g_current_vm = &g_vm;

// ---------------------------------------------------------------------------
// Frame-aware unwinding (see the comment at vm_unwind_to_boundary): every
// generic call that crosses into s7 registers a boundary -- a snapshot of
// this call's entry state.  s7_gf_apply_eval's error-landing site hands the
// boundary id back so the frames the longjmp skipped over can be dropped.
struct CallBoundary {
  uintptr_t id;             // identifies this apply_eval activation
  VM*       vm;             // g_current_vm when the call started
  size_t    frames_size;    // vm->frames.size() at entry
  size_t    stack_size;     // vm->stack.size() at entry
  size_t    vm_stack_depth; // g_vm_stack.size() at entry
};
static std::deque<CallBoundary> g_call_boundaries;
static uintptr_t g_boundary_next_id = 1;

// Normal return: drop our own boundary.  The error-landing site pops the
// boundaries it consumes (the landed-in one and everything above it), so a
// stale id simply no longer matches -- ids are never reused.
static void pop_call_boundary (uintptr_t id) {
  if (!g_call_boundaries.empty () && g_call_boundaries.back ().id == id)
    g_call_boundaries.pop_back ();
}

static VM* vm_for_prog(VMProgram* prog) {
  auto it = g_prog_map.find(prog);
  if (it != g_prog_map.end()) return it->second;
  VM* vm = new VM();
  vm->prog = prog;
  g_prog_map[prog] = vm;
  return vm;
}
static gf::pointer g_car_fn = nullptr, g_cdr_fn = nullptr, g_cons_fn = nullptr;
static gf::pointer g_eq_fn = nullptr, g_null_fn = nullptr, g_pair_fn = nullptr;
static gf::pointer g_not_fn = nullptr, g_add_fn = nullptr, g_sub_fn = nullptr;
static gf::pointer g_num_eq_fn = nullptr, g_lt_fn = nullptr;

// ---------------------------------------------------------------------------
// Stack helpers.

static inline void push (gf::pointer v) { g_current_vm->stack.push_back (v); }

static inline gf::pointer pop () {
  gf::pointer v = g_current_vm->stack.back ();
  g_current_vm->stack.pop_back ();
  return v;
}

// ---------------------------------------------------------------------------
// Global lookup: the program's resolution env first, then the rootlet.

static gf::pointer global_lookup (gf::scheme* sc, gf::pointer name, gf::pointer env) {
  if (env != nullptr && gf::is_let (env)) {
    gf::pointer v = gf::let_ref (sc, env, name);
    if (v != gf::undefined (sc)) return v;
  }
  return gf::global_value (sc, name);
}

// ---------------------------------------------------------------------------
// Function dispatch.



// ---------------------------------------------------------------------------
// Frames.

// build_args_list : args -> s7 list
static gf::pointer build_args_list (gf::scheme* sc, const std::vector<gf::pointer>& args) {
  return (args.empty ())
         ? gf::nil (sc)
         : gf::array_to_list (sc, (gf::int_)args.size (),
                             const_cast<gf::pointer*>(args.data ()));
}

// frame_from_args : f slots vector args -> void
// Fill a frame's slot vector from the call args, honoring the callee's
// formals shape: proper list, rest symbol, or dotted (fixed . rest).
// Also sets stack_base to the current stack top.
static void frame_from_args (gf::scheme* sc, VMFrame& f, const VMCodeInfo& ci,
                             const std::vector<gf::pointer>& args) {
  f.slots = gf::make_vector (sc, (gf::int_)ci.nlocals);
  if (gf::is_symbol (ci.formals)) {
    // Rest closure: the whole arg list is one slot.  The caller hands over
    // a SINGLE packed list argument (s7's rest closure bundles the args
    // into one list before vm-enter; call_function packs for VM-side calls),
    // so the slot is args[0], not a re-packaging of args.
    if (ci.nlocals > 0) {
      if (args.empty ())
        gf::vector_set (sc, f.slots, 0, gf::nil (sc));
      else
        gf::vector_set (sc, f.slots, 0, args[0]);
    }
  } else if (!gf::is_proper_list (sc, ci.formals)) {
    size_t fixed = 0;
    for (gf::pointer p = ci.formals; gf::is_pair (p); p = gf::cdr (p))
      ++fixed;
    for (size_t i = 0; i < fixed && i < args.size (); ++i)
      gf::vector_set (sc, f.slots, (gf::int_)i, args[i]);
    if ((gf::int_)fixed < ci.nlocals)
      // The rest arrives PRE-BUNDLED as the single argument after the
      // fixed ones: the s7 shell's dotted tail passes it as one list,
      // and call_function / the tail-call path bundle VM-side calls the
      // same way.  Do not re-package it here.
      gf::vector_set (sc, f.slots, (gf::int_)fixed,
                      (args.size () > fixed) ? args[fixed] : gf::nil (sc));
  } else {
    for (size_t i = 0; i < args.size () && i < (size_t)ci.nlocals; ++i)
      gf::vector_set (sc, f.slots, (gf::int_)i, args[i]);
  }
  f.stack_base = g_current_vm->stack.size ();
}

// push_frame : prog code-index args captured global-env -> void
// Push a new frame.  Its slot storage is an s7 vector (the single storage
// for this frame: Local/SetLocal and any closure's Ref/SetRef alias it).
// The frame's value region starts at the current stack top, so a nested
// VM call neither reads nor disturbs the caller's region.
static void push_frame (gf::scheme* sc, VMProgram* prog, int code_idx, const std::vector<gf::pointer>& args, gf::pointer captured, gf::pointer global_env) {
  VMCodeInfo& ci = prog->codes[code_idx];
  VMFrame f;
  f.pc = 0;
  f.code = &ci.instrs;
  f.prog = prog;
  f.global_env = global_env;
  f.captured = captured;
  frame_from_args (sc, f, ci, args);
  g_current_vm->frames.push_back (f);
}

// ---------------------------------------------------------------------------
// The interpreter loop.

// vm_closure_of : f -> VMClosure* or nullptr
// Recognizes an s7 closure whose body is (vm-enter <cobj> ...).
static VMClosure* vm_closure_of (gf::scheme* sc, gf::pointer f) {
  if (!gf::is_closure (f)) return nullptr;
  gf::pointer body = gf::closure_body (sc, f);
  if (!(gf::is_pair (body) && gf::is_pair (gf::car (body)) &&
        gf::is_eq (gf::car (gf::car (body)), vm_enter_symbol)))
    return nullptr;
  gf::pointer cobj = gf::cadr (gf::car (body));
  return static_cast<VMClosure*>(gf::c_object_value (cobj));
}

static std::vector<gf::pointer> pack_args_for_formals(gf::scheme* sc, gf::pointer formals,
                                                        const std::vector<gf::pointer>& args) {
  if (gf::is_symbol(formals)) {
    std::vector<gf::pointer> packed(1);
    packed[0] = build_args_list(sc, args);
    return packed;
  }
  if (!gf::is_proper_list(sc, formals)) {
    size_t fixed = 0;
    for (gf::pointer p = formals; gf::is_pair(p); p = gf::cdr(p)) ++fixed;
    size_t nfixed = std::min(fixed, args.size());
    std::vector<gf::pointer> packed(args.begin(), args.begin() + nfixed);
    std::vector<gf::pointer> rest(args.begin() + nfixed, args.end());
    packed.push_back(build_args_list(sc, rest));
    return packed;
  }
  return args;
}

// call_function : f (list arg) -> result or nullptr
// A VM function pushes a frame and returns nullptr (the loop continues);
// anything else is called with s7_call and its result returned.
static gf::pointer call_function (gf::scheme* sc, gf::pointer f, const std::vector<gf::pointer>& args) {
  VMClosure* vc = vm_closure_of (sc, f);
  if (vc != nullptr) {
    VMCodeInfo& ci = vc->prog->codes[vc->code_idx];
    push_frame(sc, vc->prog, vc->code_idx, pack_args_for_formals(sc, ci.formals, args), vc->captured,
               vc->global_env);
    return nullptr;
  }
  // Fast path: inline the hot primitives so a VM call does not pay for
  // building an arg list and entering s7's evaluator.  The procedure
  // objects are cached once (glue_vm); s7_is_eq is a pointer compare.
  // Every guard checks the arity before indexing args: a mismatch falls
  // through to apply_function for the proper s7 error instead of reading
  // out of bounds.  car/cdr also guard with is_pair -- the raw accessors
  // return NULL on a non-pair, which the call protocol reads as "no
  // value" and the stack underflows.
  if (gf::is_eq (f, g_car_fn) && args.size () == 1 && gf::is_pair (args[0])) return gf::car (args[0]);
  if (gf::is_eq (f, g_cdr_fn) && args.size () == 1 && gf::is_pair (args[0])) return gf::cdr (args[0]);
  if (gf::is_eq (f, g_cons_fn) && args.size () == 2) return gf::cons (sc, args[0], args[1]);
  if (gf::is_eq (f, g_eq_fn) && args.size () == 2) return gf::is_eq (args[0], args[1]) ? gf::t (sc) : gf::f (sc);
  if (gf::is_eq (f, g_null_fn) && args.size () == 1) return gf::is_null (sc, args[0]) ? gf::t (sc) : gf::f (sc);
  if (gf::is_eq (f, g_pair_fn) && args.size () == 1) return gf::is_pair (args[0]) ? gf::t (sc) : gf::f (sc);
  if (gf::is_eq (f, g_not_fn) && args.size () == 1) return (args[0] == gf::f (sc)) ? gf::t (sc) : gf::f (sc);
  // Integer fast paths for + and - (the benchmark loops); fall back to s7
  // for non-integer or overflow (s7_apply_function re-checks).
  if (gf::is_eq (f, g_add_fn)) {
    gf::int_ sum = 0;
    bool ok = !args.empty ();
    for (auto& a : args)
      if (!gf::is_integer (a)) { ok = false; break; }
      else sum += gf::integer (a);
    if (ok) return gf::make_integer (sc, sum);
  }
  if (gf::is_eq (f, g_sub_fn)) {
    if (args.size () == 1) {
      if (gf::is_integer (args[0])) return gf::make_integer (sc, -gf::integer (args[0]));
    } else {
      bool ok = true;
      for (auto& a : args)
        if (!gf::is_integer (a)) { ok = false; break; }
      if (ok) {
        gf::int_ r = gf::integer (args[0]);
        for (size_t i = 1; i < args.size (); ++i) r -= gf::integer (args[i]);
        return gf::make_integer (sc, r);
      }
    }
  }
  if (gf::is_eq (f, g_num_eq_fn)) {
    if (args.size () == 2 && gf::is_integer (args[0]) && gf::is_integer (args[1]))
      return (gf::integer (args[0]) == gf::integer (args[1])) ? gf::t (sc) : gf::f (sc);
    return gf::apply_eval (sc, f, build_args_list (sc, args));
  }
  if (gf::is_eq (f, g_lt_fn)) {
    if (args.size () == 2 && gf::is_integer (args[0]) && gf::is_integer (args[1]))
      return (gf::integer (args[0]) < gf::integer (args[1])) ? gf::t (sc) : gf::f (sc);
    return gf::apply_eval (sc, f, build_args_list (sc, args));
  }
  gf::pointer args_list = build_args_list (sc, args);
  // (apply ...) needs no special case: the generic path below goes through
  // apply_eval, whose eval-loop entry pumps s7's deferred apply opcode to a
  // value (the historical reason for hand-splicing here was that
  // s7_apply_function returned the placeholder at face value -- gone since
  // the eval-loop entry exists).  The call boundary is managed inside
  // s7_gf_apply_eval itself, so every crossing is covered.
  return gf::apply_eval (sc, f, args_list);
}
// run : target-depth -> result
static gf::pointer run (gf::scheme* sc, size_t target_depth) {
  while (g_current_vm->frames.size () > target_depth) {
    VMFrame& fr = g_current_vm->frames.back ();
    const std::vector<Instr>& code = *fr.code;
    if (fr.pc >= code.size ()) {
      // Frame ran off its instruction list (e.g. a top-level expression
      // that ends without (return)): same value-passing protocol as
      // Op::Return -- pop the frame's top-of-region value, hand it to the
      // enclosing frame's region, or return it from run at target depth.
      gf::pointer v = (g_current_vm->stack.size () > fr.stack_base) ? pop () : gf::undefined (sc);
      g_current_vm->frames.pop_back ();
      if (g_current_vm->frames.size () > target_depth) {
        push (v);
        continue;
      }
      return v;
    }
    const Instr& in = code[fr.pc++];

    switch (in.op) {
      case Op::Const:
        push (in.a);
        break;
      case Op::Global:
        push (global_lookup (sc, in.a, fr.global_env));
        break;
      case Op::Ref:
        push (gf::vector_ref (sc, gf::list_ref (sc, fr.captured, in.b - 1), in.c));
        break;
      case Op::Local:
        push (gf::vector_ref (sc, fr.slots, in.b));
        break;
      case Op::SetLocal: {
        gf::pointer v = pop ();
        gf::vector_set (sc, fr.slots, in.b, v);
        // Leave the value: every expression nets exactly one stack value,
        // so the enclosing sequence's (pop) has something to consume (same
        // convention as StoreGlobal).
        push (v);
        break;
      }
      case Op::SetRef: {
        gf::pointer v = pop ();
        gf::vector_set (sc, gf::list_ref (sc, fr.captured, in.b - 1), in.c, v);
        push (v);
        break;
      }
      case Op::StoreGlobal: {
        gf::pointer v = pop ();
        gf::pointer sym = in.a;
        // Store into the program's resolution env (an inlet such as
        // the-expander-library) when one was given, else the rootlet.
        if (fr.global_env != nullptr && gf::is_let (fr.global_env))
          gf::varlet (sc, fr.global_env, sym, v);
        else
          gf::define_variable (sc, gf::symbol_name (sym), v);
        // A (set! global ...) in non-tail position compiles as
        // const + store-global and the enclosing begin then emits (pop):
        // leave the value so that pop has something to consume.  Top-level
        // define never reads the leftover (the loaders ignore the result).
        push (v);
        break;
      }
      case Op::Closure: {
        int i = (int)in.b;
        VMCodeInfo& ci = fr.prog->codes[i];
        VMClosure* vc = new VMClosure;
        vc->prog = fr.prog;
        vc->code_idx = i;
        vc->global_env = fr.global_env;
        // Capture the frame's slot vector directly: the closure and the
        // frame share the SAME vector, so set! on a captured variable is
        // visible in both directions with no snapshot/sync machinery.
        vc->captured = gf::cons (sc, fr.slots, fr.captured);
        gf::pointer let = gf::inlet (sc,
                                   gf::cons (sc,
                                            gf::cons (sc, captured_symbol, vc->captured),
                                            gf::nil (sc)));
        gf::pointer cobj = gf::make_c_object_with_let (sc, VM_CLOSURE_TYPE, vc, let);
        gf::pointer formals = ci.formals;
        // Build the call formals as a proper list: (x y) -> (x y),
        // (a . rest) -> (a rest), a rest symbol -> (args).  The closure
        // body must be a proper list -- (vm-enter cobj a . rest) cannot be
        // evaluated -- and a rest arg arrives as one list value.
        gf::pointer call_formals;
        if (gf::is_symbol (formals)) {
          call_formals = gf::list (sc, formals);
        } else if (!gf::is_proper_list (sc, formals)) {
          gf::pointer acc = gf::nil (sc);
          gf::pointer f = formals;
          while (gf::is_pair (f)) { acc = gf::cons (sc, gf::car (f), acc); f = gf::cdr (f); }
          acc = gf::cons (sc, f, acc);
          call_formals = gf::reverse (sc, acc);
        } else {
          call_formals = formals;
        }
        gf::pointer call = gf::cons (sc, vm_enter_symbol,
                                   gf::cons (sc, cobj, call_formals));
        gf::pointer body = gf::list (sc, call);
        gf::int_ arity = gf::is_symbol (formals) ? -1
                                              : gf::list_length (sc, formals);
        push (gf::make_closure (sc, formals, body, arity));
        break;
      }
      case Op::Call:
      case Op::TailCall: {
        int n = (int)in.b;
        std::vector<gf::pointer> args (n);
        for (int i = n - 1; i >= 0; --i) args[i] = pop ();
        gf::pointer f = pop ();
        // map/for-each are implemented in Scheme (base-functions.scm,
        // Guile boot-9 style) and resolve by name into the rootlet, so the
        // VM needs no special-casing for them here: a call to map calls the
        // Scheme closure, whose callback calls go through call_function
        // (handling VM-closure callbacks correctly).
        if (in.op == Op::Call) {
          gf::pointer r = call_function (sc, f, args);
          if (r != nullptr) push (r);
        } else {
          // Tail call.  To a VM closure: replace the CURRENT frame in place
          // (true tail call -- no frame churn, no dangling fr reference).
          // To anything else: the current frame ends; hand the callee's
          // value to the enclosing frame's region (or return it from run at
          // the target depth).
            VMClosure* vc = vm_closure_of (sc, f);
            if (vc != nullptr) {
              VMCodeInfo& ci = vc->prog->codes[vc->code_idx];
              fr.pc = 0;
              fr.code = &ci.instrs;
              fr.prog = vc->prog;
              fr.global_env = vc->global_env;
              fr.captured = vc->captured;
              frame_from_args(sc, fr, ci, pack_args_for_formals(sc, ci.formals, args));
              continue;
            }
          gf::pointer r = call_function (sc, f, args);
          g_current_vm->frames.pop_back ();
          if (g_current_vm->frames.size () > target_depth) {
            if (r != nullptr) push (r);
            continue;  // re-bind fr (the loop's fr reference is dangling)
          }
          // No enclosing frame below target depth.  If the callee pushed a
          // VM frame (r == nullptr), keep running inside it (the loop sees
          // g_current_vm->frames.size() > target_depth again and continues); otherwise
          // return the callee's value.
          if (r == nullptr)
            continue;
          return r;
        }
        break;
      }
      case Op::IfElse: {
        gf::pointer t = pop ();
        if (t == g_false)
          fr.pc = (size_t)in.b;
        break;
      }
      case Op::Jump:
        fr.pc = (size_t)in.b;
        break;
      case Op::Return: {
        // End of the current frame: pop the frame's top-of-region value
        // (undefined if empty) and hand it to the enclosing frame's
        // region, or return it from run if none remains.
        gf::pointer v = (g_current_vm->stack.size () > fr.stack_base) ? pop () : gf::undefined (sc);
        g_current_vm->frames.pop_back ();
        if (g_current_vm->frames.size () > target_depth) {
          push (v);
          continue;  // re-bind fr; the caller's frame resumes with v on top
        }
        return v;
      }
      case Op::Pop:
        pop ();
        break;
      default:
        g_current_vm->frames.pop_back ();
        return gf::error (sc, gf::make_symbol (sc, "vm-error"),
                         gf::list (sc, gf::make_string (sc, "unknown instruction"),
                                  gf::make_integer (sc, (gf::int_)in.op)));
    }
  }
  return gf::undefined (sc);
}

// ---------------------------------------------------------------------------
// vm-load : program env -> top result
// Load a program, set its (global ...) resolution environment (an inlet
// such as the-expander-library, or #f for the rootlet), run the top
// instruction list, and return the value it leaves on the stack.
static gf::pointer vm_load (gf::scheme* sc, gf::pointer args) {
  gf::pointer program = gf::car (args);
  gf::gc_protect (sc, program);
  gf::pointer global_env = gf::cadr (args);
  VMProgram* p = new VMProgram;
  gf::pointer ctable = gf::cadr (program);
  for (gf::pointer c = gf::cdr (ctable); gf::is_pair (c); c = gf::cdr (c)) {
    gf::pointer code = gf::car (c);
    VMCodeInfo ci;
    ci.nlocals = gf::integer (gf::cadr (code));
    ci.formals = gf::caddr (code);
    ci.instrs = decode_instrs (sc, gf::cadddr (code));
    p->codes.push_back (ci);
  }
  gf::pointer top = gf::caddr (program);
  // (top <nlocals> <instr-vector>) -- slot count for top-level
  // expressions (a top-level let's bindings are captured by lambdas).
  gf::int_ top_nlocals = gf::integer (gf::cadr (top));
  p->top = decode_instrs (sc, gf::caddr (top));
  VM* vm = vm_for_prog(p);
  g_vm_stack.push_back(g_current_vm);
  g_current_vm = vm;
  g_current_vm->prog = p;
  g_current_vm->stack.clear ();
  g_current_vm->frames.clear ();
  VMFrame f;
  f.pc = 0;
  f.code = &p->top;
  f.prog = p;
  f.global_env = global_env;
  f.captured = gf::nil (sc);
  f.stack_base = 0;
  f.slots = gf::make_vector (sc, top_nlocals);
  g_current_vm->frames.push_back (f);
  bool saved_gc = gf::gc_enabled (sc);
  gf::gc_on (sc, false);
  gf::pointer result = run (sc, 0);
  gf::gc_on (sc, saved_gc);
  g_current_vm = g_vm_stack.back();
  g_vm_stack.pop_back();
  return result;
}

// ---------------------------------------------------------------------------
// vm-enter : (cobj . args) -> result
static gf::pointer vm_enter (gf::scheme* sc, gf::pointer args) {
  gf::pointer cobj = gf::car (args);
  VMClosure* vc = static_cast<VMClosure*>(gf::c_object_value (cobj));
  VM* target = vm_for_prog(vc->prog);
  g_vm_stack.push_back(g_current_vm);
  g_current_vm = target;
  bool saved_gc = gf::gc_enabled (sc);
  gf::gc_on (sc, false);
  size_t d0 = g_current_vm->frames.size ();
  std::vector<gf::pointer> arg_list;
  for (gf::pointer a = gf::cdr (args); gf::is_pair (a); a = gf::cdr (a))
    arg_list.push_back (gf::car (a));
  push_frame (sc, vc->prog, vc->code_idx, arg_list, vc->captured, vc->global_env);
  gf::pointer result = run (sc, d0);
  gf::gc_on (sc, saved_gc);
  g_current_vm = g_vm_stack.back();
  g_vm_stack.pop_back();
  return result;
}

// ---------------------------------------------------------------------------
// Frame-aware unwinding.
//
// s7 recovers from errors by longjmp'ing to the jump buffer captured when
// the innermost `catch` (or s7_call) was installed.  When that catch lives
// in VM-compiled code, the buffer belongs to an apply_eval activation
// launched from call_function -- and the jump flies over every C++ frame
// above it: nested run() loops, vm_enter cleanups, their frames deque and
// stack regions.  Without repair, s7 then runs the error handler at the
// landed buffer; its result flows back through call_function as if it were
// the failed primitive's ordinary value, and the stale frames corrupt
// everything after (the historical "error replay" bug: njson-ref-test,
// srfi-165-test).
//
// The protocol: each apply_eval activation registers a boundary snapshot on
// the way in.  s7_gf_apply_eval's landing site calls this function with its
// boundary id before running any handler; we drop every boundary at or
// above the matching one and truncate each touched VM's state to the oldest
// surviving snapshot.  The handler therefore executes against exactly the
// state the catching context expects, and the normal return path of the
// landed-in activation still works (ids never repeat, so its pop is a no-op).
void vm_unwind_to_boundary (std::uintptr_t id) {
  // Locate the matching boundary; entries above it are dead by definition.
  size_t target = g_call_boundaries.size ();
  for (size_t i = g_call_boundaries.size (); i > 0; --i)
    if (g_call_boundaries[i - 1].id == static_cast<uintptr_t> (id)) { target = i - 1; break; }
  if (target == g_call_boundaries.size ()) return; // unknown id: nothing to reclaim

  // Truncate each touched VM once, to the OLDEST dead snapshot referencing
  // it (nested activations share one VM instance per program).
  for (size_t i = target; i < g_call_boundaries.size (); ++i) {
    const CallBoundary& b = g_call_boundaries[i];
    VM* vm = b.vm;
    if (vm->frames.size () > b.frames_size) vm->frames.resize (b.frames_size);
    if (vm->stack.size () > b.stack_size) vm->stack.resize (b.stack_size);
  }

  // Restore the instance switch the skipped vm_enter/vm_load tails would
  // have performed: the matched boundary's vm becomes current again, and
  // instance stack entries pushed after its snapshot are orphaned.
  const CallBoundary& tb = g_call_boundaries[target];
  if (g_vm_stack.size () > tb.vm_stack_depth) {
    g_vm_stack.resize (tb.vm_stack_depth);
    g_current_vm = tb.vm;
  }

  g_call_boundaries.resize (target);
}

// Protocol entry points, called from s7_gf_apply_eval (declared in s7.h):
// push on the way into a generic call, pop on its normal return, unwind
// from the error-landing site before any handler runs.  C linkage: s7.c is
// a C translation unit and expects unmangled symbols.
} // namespace goldfish

extern "C" {

// (using goldfish:: qualification: the boundary machinery is file-local to
// the goldfish namespace, only these entry points cross into s7.c)

uintptr_t goldfish_vm_push_boundary () {
  using namespace goldfish;
  g_call_boundaries.push_back (CallBoundary{g_boundary_next_id, g_current_vm,
                                             g_current_vm->frames.size (),
                                             g_current_vm->stack.size (),
                                             g_vm_stack.size ()});
  return g_boundary_next_id++;
}

void goldfish_vm_pop_boundary (std::uintptr_t id) { goldfish::pop_call_boundary (id); }

void s7_gf_vm_unwind (std::uintptr_t id) { goldfish::vm_unwind_to_boundary (id); }

}

namespace goldfish {
// ---------------------------------------------------------------------------

void glue_vm (gf::scheme* sc) {
  VM_CLOSURE_TYPE = gf::make_c_type (sc, "vm-closure");
  g_car_fn   = gf::global_value (sc, gf::make_symbol (sc, "car"));
  g_cdr_fn   = gf::global_value (sc, gf::make_symbol (sc, "cdr"));
  g_cons_fn  = gf::global_value (sc, gf::make_symbol (sc, "cons"));
  g_eq_fn    = gf::global_value (sc, gf::make_symbol (sc, "eq?"));
  g_null_fn  = gf::global_value (sc, gf::make_symbol (sc, "null?"));
  g_pair_fn  = gf::global_value (sc, gf::make_symbol (sc, "pair?"));
  g_not_fn   = gf::global_value (sc, gf::make_symbol (sc, "not"));
  g_add_fn   = gf::global_value (sc, gf::make_symbol (sc, "+"));
  g_sub_fn   = gf::global_value (sc, gf::make_symbol (sc, "-"));
  g_num_eq_fn = gf::global_value (sc, gf::make_symbol (sc, "="));
  g_lt_fn    = gf::global_value (sc, gf::make_symbol (sc, "<"));
  g_false = gf::f (sc);
  vm_enter_symbol  = gf::make_symbol (sc, "vm-enter");
  quote_symbol     = gf::make_symbol (sc, "quote");
  captured_symbol  = gf::make_symbol (sc, "*vm-captured*");

  gf::define_function (sc, "vm-load", vm_load, 2, 0, false,
                      "(vm-load program global-env)");
  gf::define_function (sc, "vm-enter", vm_enter, 1, 0, true, "(vm-enter cobj . args)");
}

} // namespace goldfish
