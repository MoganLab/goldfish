//
// goldfish_vm.cpp -- the self-hosted bytecode VM.
//
// Executes the bytecode emitted by (goldfish compiler bytecode):
//   (program (code-table (code nlocals formals instr...) ...) (top instr...))
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
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

namespace goldfish {

static gf::int_ VM_CLOSURE_TYPE = 0;

// ---------------------------------------------------------------------------
// Decoded instruction.

enum class Op : uint8_t {
  Const, Global, Ref, Local, SetLocal, SetRef, StoreGlobal,
  Closure, Call, TailCall, IfElse, Jump, Label, Return, Pop,
  Values, CallWithValues, Unknown
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
static gf::pointer vm_const_symbol  = nullptr;
static gf::pointer vm_global_symbol = nullptr;
static gf::pointer vm_ref_symbol    = nullptr;
static gf::pointer vm_local_symbol  = nullptr;
static gf::pointer vm_set_local_symbol = nullptr;
static gf::pointer vm_set_ref_symbol = nullptr;
static gf::pointer vm_store_global_symbol = nullptr;
static gf::pointer vm_closure_symbol = nullptr;
static gf::pointer vm_call_symbol   = nullptr;
static gf::pointer vm_tail_call_symbol = nullptr;
static gf::pointer vm_if_else_symbol = nullptr;
static gf::pointer vm_jump_symbol   = nullptr;
static gf::pointer vm_label_symbol  = nullptr;
static gf::pointer vm_return_symbol = nullptr;
static gf::pointer vm_pop_symbol    = nullptr;
static gf::pointer vm_values_symbol = nullptr;
static gf::pointer vm_call_with_values_symbol = nullptr;

static Op decode_op (gf::pointer sym) {
  if (gf::is_eq (sym, vm_const_symbol)) return Op::Const;
  if (gf::is_eq (sym, vm_global_symbol)) return Op::Global;
  if (gf::is_eq (sym, vm_ref_symbol)) return Op::Ref;
  if (gf::is_eq (sym, vm_local_symbol)) return Op::Local;
  if (gf::is_eq (sym, vm_set_local_symbol)) return Op::SetLocal;
  if (gf::is_eq (sym, vm_set_ref_symbol)) return Op::SetRef;
  if (gf::is_eq (sym, vm_store_global_symbol)) return Op::StoreGlobal;
  if (gf::is_eq (sym, vm_closure_symbol)) return Op::Closure;
  if (gf::is_eq (sym, vm_call_symbol)) return Op::Call;
  if (gf::is_eq (sym, vm_tail_call_symbol)) return Op::TailCall;
  if (gf::is_eq (sym, vm_if_else_symbol)) return Op::IfElse;
  if (gf::is_eq (sym, vm_jump_symbol)) return Op::Jump;
  if (gf::is_eq (sym, vm_label_symbol)) return Op::Label;
  if (gf::is_eq (sym, vm_return_symbol)) return Op::Return;
  if (gf::is_eq (sym, vm_pop_symbol)) return Op::Pop;
  if (gf::is_eq (sym, vm_values_symbol)) return Op::Values;
  if (gf::is_eq (sym, vm_call_with_values_symbol)) return Op::CallWithValues;
  return Op::Unknown;
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

static VM* vm_for_prog(VMProgram* prog) {
  auto it = g_prog_map.find(prog);
  if (it != g_prog_map.end()) return it->second;
  VM* vm = new VM();
  vm->prog = prog;
  g_prog_map[prog] = vm;
  return vm;
}
static gf::pointer g_apply_fn = nullptr;  // the rootlet 'apply procedure
static gf::pointer g_car_fn = nullptr, g_cdr_fn = nullptr, g_cons_fn = nullptr;
static gf::pointer g_eq_fn = nullptr, g_null_fn = nullptr, g_pair_fn = nullptr;
static gf::pointer g_not_fn = nullptr, g_add_fn = nullptr, g_sub_fn = nullptr;
static gf::pointer g_num_eq_fn = nullptr, g_lt_fn = nullptr;
static gf::pointer g_call_with_values_fn = nullptr;

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
// Instruction decoding.

static std::vector<Instr> decode_instrs (gf::scheme* sc, gf::pointer instr_list) {
  std::vector<Instr> v;
  for (gf::pointer p = instr_list; gf::is_pair (p); p = gf::cdr (p)) {
    gf::pointer instr = gf::car (p);
    if (!gf::is_pair (instr)) continue;
    Instr in;
    in.op = decode_op (gf::car (instr));
    switch (in.op) {
      case Op::Const:
        // core->ir already unfolds a (quote X) datum into its value, so the
        // operand is the value itself -- even when that value happens to be
        // a (quote ...) form (e.g. (quote (quote infix)) from a self-hosted
        // expander).  Do NOT re-unwrap here: it would over-unwrap such a
        // nested quote into the bare inner atom.
        in.a = gf::cadr (instr);
        break;
      case Op::Global:
      case Op::StoreGlobal:
        in.a = gf::cadr (instr);
        break;
      case Op::Ref:
      case Op::SetRef:
        in.b = gf::integer (gf::cadr (instr));
        in.c = gf::integer (gf::caddr (instr));
        break;
      case Op::Local:
      case Op::SetLocal:
      case Op::Closure:
      case Op::Call:
      case Op::TailCall:
      case Op::Values:
        in.b = gf::integer (gf::cadr (instr));
        break;
      case Op::IfElse:
      case Op::Jump:
      case Op::Label:
        in.b = gf::integer (gf::cadr (instr));
        break;
      case Op::Return:
      case Op::Pop:
      case Op::CallWithValues:
        break;
      default:
        break;
    }
    v.push_back (in);
  }
  // resolve jump targets to instruction indices
  std::map<gf::int_, size_t> labels;
  for (size_t i = 0; i < v.size (); ++i)
    if (v[i].op == Op::Label)
      labels[v[i].b] = i;
  for (size_t i = 0; i < v.size (); ++i)
    if (v[i].op == Op::IfElse || v[i].op == Op::Jump)
      v[i].b = (gf::int_)labels[v[i].b];
  return v;
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
    if ((gf::int_)fixed < ci.nlocals) {
      std::vector<gf::pointer> rest_args;
      for (size_t i = fixed; i < args.size (); ++i)
        rest_args.push_back (args[i]);
      gf::vector_set (sc, f.slots, (gf::int_)fixed, build_args_list (sc, rest_args));
    }
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

// call_function : f (list arg) -> result or nullptr
// A VM function pushes a frame and returns nullptr (the loop continues);
// anything else is called with s7_call and its result returned.
static gf::pointer call_function (gf::scheme* sc, gf::pointer f, const std::vector<gf::pointer>& args) {
  VMClosure* vc = vm_closure_of (sc, f);
  if (vc != nullptr) {
    VMCodeInfo& ci = vc->prog->codes[vc->code_idx];
    if (gf::is_symbol (ci.formals)) {
      // Rest closure: bundle the raw call args into one list, matching what
      // s7's rest shell hands to vm-enter (frame_from_args expects the
      // single packed list in args[0]).
      std::vector<gf::pointer> packed (1);
      packed[0] = build_args_list (sc, args);
      push_frame (sc, vc->prog, vc->code_idx, packed, vc->captured, vc->global_env);
    } else {
      push_frame (sc, vc->prog, vc->code_idx, args, vc->captured, vc->global_env);
    }
    return nullptr;
  }
  // Fast path: inline the hot primitives so a VM call does not pay for
  // building an arg list and entering s7's evaluator.  The procedure
  // objects are cached once (glue_vm); s7_is_eq is a pointer compare.
  if (gf::is_eq (f, g_car_fn)) return gf::car (args[0]);
  if (gf::is_eq (f, g_cdr_fn)) return gf::cdr (args[0]);
  if (gf::is_eq (f, g_cons_fn)) return gf::cons (sc, args[0], args[1]);
  if (gf::is_eq (f, g_eq_fn)) return gf::is_eq (args[0], args[1]) ? gf::t (sc) : gf::f (sc);
  if (gf::is_eq (f, g_null_fn)) return gf::is_null (sc, args[0]) ? gf::t (sc) : gf::f (sc);
  if (gf::is_eq (f, g_pair_fn)) return gf::is_pair (args[0]) ? gf::t (sc) : gf::f (sc);
  if (gf::is_eq (f, g_not_fn)) return (args[0] == gf::f (sc)) ? gf::t (sc) : gf::f (sc);
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
    if (gf::is_integer (args[0]) && gf::is_integer (args[1]))
      return (gf::integer (args[0]) == gf::integer (args[1])) ? gf::t (sc) : gf::f (sc);
    return gf::apply_function (sc, f, build_args_list (sc, args));
  }
  if (gf::is_eq (f, g_lt_fn)) {
    if (gf::is_integer (args[0]) && gf::is_integer (args[1]))
      return (gf::integer (args[0]) < gf::integer (args[1])) ? gf::t (sc) : gf::f (sc);
    return gf::apply_function (sc, f, build_args_list (sc, args));
  }
  gf::pointer args_list = build_args_list (sc, args);
  // s7's apply primitive is a deferred opcode: g_apply pushes OP_APPLY onto
  // the evaluator stack and returns sc->nil, leaving the real call for the
  // eval loop.  s7_apply_function therefore returns () for (apply ...) --
  // harmless inside a normal s7 eval, but the VM interpreter loop takes the
  // return value at face value, so an (apply ...) from VM bytecode would
  // deliver () to the next instruction (e.g. datum->syntax y ()).  Also, a
  // closure body reached through the deferred path mis-handles let-scoped
  // mutations (typed-lambda's do/set-car! on its arg list).  Implement
  // (apply proc a1 ... an) directly instead: splice the final list
  // argument into the argument list and call the procedure.
  if (gf::is_eq (f, g_apply_fn)) {
    gf::pointer proc = gf::car (args_list);
    gf::pointer rest = gf::cdr (args_list);            // (a1 ... an)
    if (!gf::is_pair (rest))                          // (apply proc) -- no args
      return gf::apply_function (sc, proc, gf::nil (sc));
    gf::pointer p = rest;
    while (gf::is_pair (gf::cdr (p))) p = gf::cdr (p);  // p is the last cons (an)
    gf::pointer last = gf::car (p);
    gf::pointer spliced;
    if (gf::is_null (sc, last)) {
      if (rest == p)                                 // only (proc ()) -> no args
        spliced = gf::nil (sc);
      else {                                         // drop the () tail
        gf::pointer q = rest;
        while (gf::is_pair (gf::cdr (q)) && gf::cdr (q) != p) q = gf::cdr (q);
        gf::set_cdr (q, gf::nil (sc));
        spliced = rest;
      }
    } else {
      if (rest == p)                                 // only (proc list) -> splice
        spliced = last;
      else {                                         // splice list after fixed args
        gf::pointer q = rest;
        while (gf::is_pair (gf::cdr (q)) && gf::cdr (q) != p) q = gf::cdr (q);
        gf::set_cdr (q, last);
        spliced = rest;
      }
    }
    return gf::apply_function (sc, proc, spliced);
  }
  return gf::apply_function (sc, f, args_list);
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
        break;
      }
      case Op::SetRef:
        gf::vector_set (sc, gf::list_ref (sc, fr.captured, in.b - 1), in.c, pop ());
        break;
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
            if (gf::is_symbol (ci.formals)) {
              // Rest closure: bundle the raw call args like call_function.
              std::vector<gf::pointer> packed (1);
              packed[0] = build_args_list (sc, args);
              frame_from_args (sc, fr, ci, packed);
            } else {
              frame_from_args (sc, fr, ci, args);
            }
            continue;  // loop re-reads fr (same object, but code/stack change)
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
      case Op::Label:
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
      case Op::Values: {
        int n = (int)in.b;
        gf::pointer args_list = gf::nil (sc);
        for (int i = 0; i < n; ++i) args_list = gf::cons (sc, pop (), args_list);
        push (gf::values (sc, args_list));
        break;
      }
      case Op::CallWithValues: {
        gf::pointer c = pop ();
        gf::pointer p = pop ();
        // Always delegate to s7's call-with-values.  Its splice happens on
        // the s7 evaluator stack, so a multi-value producer result (whether
        // a plain or VM closure) is correctly spliced into the consumer's
        // arguments.  A manual VM unwrap cannot work: s7_values called from
        // the VM (not an s7 eval context) returns an object whose multiple
        // value flag is not reliably set, so gf::is_multiple_value would
        // miss it and the consumer would receive a single bogus argument.
        gf::pointer cwv = g_call_with_values_fn;
        if (cwv == nullptr || cwv == gf::undefined (sc))
          cwv = gf::name_to_value (sc, "call-with-values");
        gf::pointer arg_list = gf::cons (sc, p, gf::cons (sc, c, gf::nil (sc)));
        push (gf::apply_function (sc, cwv, arg_list));
        break;
      }
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
  gf::int_ top_nlocals = 0;
  if (gf::is_pair (gf::cdr (top)) && gf::is_integer (gf::cadr (top))) {
    // (top <nlocals> instr...) -- slot count for top-level expressions
    // (a top-level let's bindings are captured by lambdas).
    top_nlocals = gf::integer (gf::cadr (top));
    p->top = decode_instrs (sc, gf::cddr (top));
  } else {
    // Legacy (top instr...): no top-level slots.
    p->top = decode_instrs (sc, gf::cdr (top));
  }
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

void glue_vm (gf::scheme* sc) {
  VM_CLOSURE_TYPE = gf::make_c_type (sc, "vm-closure");
  g_apply_fn = gf::global_value (sc, gf::make_symbol (sc, "apply"));
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
  g_call_with_values_fn = gf::name_to_value (sc, "call-with-values");
  g_false = gf::f (sc);
  vm_enter_symbol  = gf::make_symbol (sc, "vm-enter");
  quote_symbol     = gf::make_symbol (sc, "quote");
  captured_symbol  = gf::make_symbol (sc, "*vm-captured*");
  vm_const_symbol  = gf::make_symbol (sc, "const");
  vm_global_symbol = gf::make_symbol (sc, "global");
  vm_ref_symbol    = gf::make_symbol (sc, "ref");
  vm_local_symbol  = gf::make_symbol (sc, "local");
  vm_set_local_symbol = gf::make_symbol (sc, "set-local");
  vm_set_ref_symbol = gf::make_symbol (sc, "set-ref");
  vm_store_global_symbol = gf::make_symbol (sc, "store-global");
  vm_closure_symbol = gf::make_symbol (sc, "closure");
  vm_call_symbol   = gf::make_symbol (sc, "call");
  vm_tail_call_symbol = gf::make_symbol (sc, "tail-call");
  vm_if_else_symbol = gf::make_symbol (sc, "if-else");
  vm_jump_symbol   = gf::make_symbol (sc, "jump");
  vm_label_symbol  = gf::make_symbol (sc, "label");
  vm_return_symbol = gf::make_symbol (sc, "return");
  vm_pop_symbol    = gf::make_symbol (sc, "pop");
  vm_values_symbol = gf::make_symbol (sc, "values");
  vm_call_with_values_symbol = gf::make_symbol (sc, "call-with-values");

  gf::define_function (sc, "vm-load", vm_load, 2, 0, false,
                      "(vm-load program global-env)");
  gf::define_function (sc, "vm-enter", vm_enter, 1, 0, true, "(vm-enter cobj . args)");
}

} // namespace goldfish
