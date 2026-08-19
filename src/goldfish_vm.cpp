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
// The instruction stream is pre-decoded at load time into C++ structs
// (enum opcode + fixed operands, jump targets resolved to indices), so
// the interpreter loop is a plain switch -- no s7 list walking at run
// time.  Calls between VM functions (including tail calls) are plain
// jumps.  GC is disabled while the VM runs (its stacks hold s7_pointer
// values in C++ vectors the conservative GC cannot see); captured slot
// vectors live in the c_object's let, which the GC marks, so closures
// survive across runs.  The loaded program datum is gc-protected for
// the lifetime of the process (one program at a time, v1).

#include "s7.h"
#include <map>
#include <string>
#include <vector>

namespace goldfish {

static s7_int VM_CLOSURE_TYPE = 0;

// ---------------------------------------------------------------------------
// Decoded instruction.

enum class Op : uint8_t {
  Const, Global, Ref, Local, SetLocal, SetRef, StoreGlobal,
  Closure, Call, TailCall, IfElse, Jump, Label, Return, Pop,
  Values, CallWithValues, Unknown
};

struct Instr {
  Op op = Op::Unknown;
  s7_pointer a = nullptr;  // symbol/constant operand
  s7_int b = 0;            // integer operand (depth/arity/label)
  s7_int c = 0;            // second integer (slot index for ref/set-ref)
};

static s7_pointer vm_enter_symbol  = nullptr;  // 'vm-enter
static s7_pointer quote_symbol     = nullptr;  // 'quote
static s7_pointer captured_symbol  = nullptr;  // '*vm-captured*
static s7_pointer g_false = nullptr;           // cached #f (unique object)
static s7_pointer vm_const_symbol  = nullptr;
static s7_pointer vm_global_symbol = nullptr;
static s7_pointer vm_ref_symbol    = nullptr;
static s7_pointer vm_local_symbol  = nullptr;
static s7_pointer vm_set_local_symbol = nullptr;
static s7_pointer vm_set_ref_symbol = nullptr;
static s7_pointer vm_store_global_symbol = nullptr;
static s7_pointer vm_closure_symbol = nullptr;
static s7_pointer vm_call_symbol   = nullptr;
static s7_pointer vm_tail_call_symbol = nullptr;
static s7_pointer vm_if_else_symbol = nullptr;
static s7_pointer vm_jump_symbol   = nullptr;
static s7_pointer vm_label_symbol  = nullptr;
static s7_pointer vm_return_symbol = nullptr;
static s7_pointer vm_pop_symbol    = nullptr;
static s7_pointer vm_values_symbol = nullptr;
static s7_pointer vm_call_with_values_symbol = nullptr;

static Op decode_op (s7_pointer sym) {
  if (s7_is_eq (sym, vm_const_symbol)) return Op::Const;
  if (s7_is_eq (sym, vm_global_symbol)) return Op::Global;
  if (s7_is_eq (sym, vm_ref_symbol)) return Op::Ref;
  if (s7_is_eq (sym, vm_local_symbol)) return Op::Local;
  if (s7_is_eq (sym, vm_set_local_symbol)) return Op::SetLocal;
  if (s7_is_eq (sym, vm_set_ref_symbol)) return Op::SetRef;
  if (s7_is_eq (sym, vm_store_global_symbol)) return Op::StoreGlobal;
  if (s7_is_eq (sym, vm_closure_symbol)) return Op::Closure;
  if (s7_is_eq (sym, vm_call_symbol)) return Op::Call;
  if (s7_is_eq (sym, vm_tail_call_symbol)) return Op::TailCall;
  if (s7_is_eq (sym, vm_if_else_symbol)) return Op::IfElse;
  if (s7_is_eq (sym, vm_jump_symbol)) return Op::Jump;
  if (s7_is_eq (sym, vm_label_symbol)) return Op::Label;
  if (s7_is_eq (sym, vm_return_symbol)) return Op::Return;
  if (s7_is_eq (sym, vm_pop_symbol)) return Op::Pop;
  if (s7_is_eq (sym, vm_values_symbol)) return Op::Values;
  if (s7_is_eq (sym, vm_call_with_values_symbol)) return Op::CallWithValues;
  return Op::Unknown;
}

struct VMProgram;
struct VMClosure {
  VMProgram* prog;     // the program this closure's code lives in
  int code_idx;
  s7_pointer captured;  // list of enclosing slot vectors (outermost first)
  s7_pointer global_env;  // the (global ...) resolution env of this program
};

struct VMCodeInfo {
  int nlocals;
  s7_pointer formals;
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
                              // global g_program, which a nested vm-load can
                              // replace mid-execution)
  std::vector<s7_pointer> slots;  // frame slots (fast Local/SetLocal)
  s7_pointer shared_slots = nullptr;  // lazy s7-vector snapshot for closure
                                      // capture; SetLocal keeps it in sync so
                                      // letrec closures see post-init values
  s7_pointer captured; // list of enclosing slot vectors (outermost first)
  s7_pointer global_env = nullptr;  // (global ...) resolution env of this frame
};

static VMProgram* g_program = nullptr;
static std::vector<s7_pointer> g_stack;
static std::vector<VMFrame> g_frames;
static s7_pointer g_apply_fn = nullptr;  // the rootlet 'apply procedure
static s7_pointer g_car_fn = nullptr, g_cdr_fn = nullptr, g_cons_fn = nullptr;
static s7_pointer g_eq_fn = nullptr, g_null_fn = nullptr, g_pair_fn = nullptr;
static s7_pointer g_not_fn = nullptr, g_add_fn = nullptr, g_sub_fn = nullptr;
static s7_pointer g_num_eq_fn = nullptr, g_lt_fn = nullptr;

// ---------------------------------------------------------------------------
// Stack helpers.

static inline void push (s7_pointer v) { g_stack.push_back (v); }

static inline s7_pointer pop () {
  s7_pointer v = g_stack.back ();
  g_stack.pop_back ();
  return v;
}

// ---------------------------------------------------------------------------
// Global lookup: the program's resolution env first, then the rootlet.

static s7_pointer global_lookup (s7_scheme* sc, s7_pointer name, s7_pointer env) {
  if (env != nullptr && s7_is_let (env)) {
    s7_pointer v = s7_let_ref (sc, env, name);
    if (v != s7_undefined (sc)) return v;
  }
  return s7_gf_global_value (sc, name);
}

// ---------------------------------------------------------------------------
// Instruction decoding.

// unwrap_quote : v -> v'
// A (const (quote x)) operand is stored as x; everything else as-is.
static s7_pointer unwrap_quote (s7_pointer v) {
  if (s7_is_pair (v) && s7_is_eq (s7_car (v), quote_symbol))
    return s7_cadr (v);
  return v;
}

static std::vector<Instr> decode_instrs (s7_scheme* sc, s7_pointer instr_list) {
  std::vector<Instr> v;
  for (s7_pointer p = instr_list; s7_is_pair (p); p = s7_cdr (p)) {
    s7_pointer instr = s7_car (p);
    if (!s7_is_pair (instr)) continue;
    Instr in;
    in.op = decode_op (s7_car (instr));
    switch (in.op) {
      case Op::Const:
        in.a = unwrap_quote (s7_cadr (instr));
        break;
      case Op::Global:
      case Op::StoreGlobal:
        in.a = s7_cadr (instr);
        break;
      case Op::Ref:
      case Op::SetRef:
        in.b = s7_integer (s7_cadr (instr));
        in.c = s7_integer (s7_caddr (instr));
        break;
      case Op::Local:
      case Op::SetLocal:
      case Op::Closure:
      case Op::Call:
      case Op::TailCall:
      case Op::Values:
        in.b = s7_integer (s7_cadr (instr));
        break;
      case Op::IfElse:
      case Op::Jump:
      case Op::Label:
        in.b = s7_integer (s7_cadr (instr));
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
  std::map<s7_int, size_t> labels;
  for (size_t i = 0; i < v.size (); ++i)
    if (v[i].op == Op::Label)
      labels[v[i].b] = i;
  for (size_t i = 0; i < v.size (); ++i)
    if (v[i].op == Op::IfElse || v[i].op == Op::Jump)
      v[i].b = (s7_int)labels[v[i].b];
  return v;
}

// ---------------------------------------------------------------------------
// Function dispatch.



// ---------------------------------------------------------------------------
// Frames.

static void push_frame (s7_scheme* sc, VMProgram* prog, int code_idx, const std::vector<s7_pointer>& args, s7_pointer captured, s7_pointer global_env, std::vector<s7_pointer>* reuse) {
  VMCodeInfo& ci = prog->codes[code_idx];
  VMFrame f;
  f.pc = 0;
  f.code = &ci.instrs;
  f.prog = prog;
  f.global_env = global_env;
  if (reuse != nullptr) f.slots = std::move (*reuse);
  f.slots.assign (ci.nlocals, s7_nil (sc));
  for (size_t i = 0; i < args.size () && i < (size_t)ci.nlocals; ++i)
    f.slots[i] = args[i];
  f.shared_slots = nullptr;
  f.captured = captured;
  g_frames.push_back (f);
}

// snapshot_slots : frame -> s7 vector
// The frame's slot vector SHARED (lazily materialized on first capture):
// letrec init closures must see the bindings their siblings set after they
// are built, so SetLocal keeps shared_slots in sync with the frame slots.
static s7_pointer snapshot_slots (s7_scheme* sc, const VMFrame& fr) {
  return fr.shared_slots;
}

// ---------------------------------------------------------------------------
// The interpreter loop.

// build_args_list : args -> s7 list
static s7_pointer build_args_list (s7_scheme* sc, const std::vector<s7_pointer>& args) {
  return (args.empty ())
         ? s7_nil (sc)
         : s7_array_to_list (sc, (s7_int)args.size (),
                             const_cast<s7_pointer*>(args.data ()));
}

// call_function : f (list arg) reuse-slots -> result or nullptr
// A VM function pushes a frame and returns nullptr (the loop continues);
// anything else is called with s7_call and its result returned.  reuse
// (optional) lets a tail call hand its slot array to the new frame.
static s7_pointer call_function (s7_scheme* sc, s7_pointer f, const std::vector<s7_pointer>& args, std::vector<s7_pointer>* reuse) {
  if (s7_gf_is_closure (f)) {
    s7_pointer body = s7_closure_body (sc, f);
    if (s7_is_pair (body) && s7_is_pair (s7_car (body)) &&
        s7_is_eq (s7_car (s7_car (body)), vm_enter_symbol)) {
      s7_pointer cobj = s7_cadr (s7_car (body));
      VMClosure* vc = static_cast<VMClosure*>(s7_c_object_value (cobj));
      VMCodeInfo& ci = vc->prog->codes[vc->code_idx];
      if (s7_is_symbol (ci.formals)) {
        // Rest closure: pack ALL arguments into a single list argument.
        std::vector<s7_pointer> packed (1);
        packed[0] = build_args_list (sc, args);
        push_frame (sc, vc->prog, vc->code_idx, packed, vc->captured, vc->global_env, reuse);
      } else if (!s7_is_proper_list (sc, ci.formals)) {
        // Dotted formals (fixed . rest): fixed params then one list for the rest.
        size_t fixed = 0;
        for (s7_pointer p = ci.formals; s7_is_pair (p); p = s7_cdr (p))
          ++fixed;
        std::vector<s7_pointer> packed (fixed + 1);
        for (size_t i = 0; i < fixed; ++i)
          packed[i] = (i < args.size ()) ? args[i] : s7_nil (sc);
        std::vector<s7_pointer> rest_args;
        for (size_t i = fixed; i < args.size (); ++i)
          rest_args.push_back (args[i]);
        packed[fixed] = build_args_list (sc, rest_args);
        push_frame (sc, vc->prog, vc->code_idx, packed, vc->captured, vc->global_env, reuse);
      } else {
        push_frame (sc, vc->prog, vc->code_idx, args, vc->captured, vc->global_env, reuse);
      }
      return nullptr;
    }
  }
  // Fast path: inline the hot primitives so a VM call does not pay for
  // building an arg list and entering s7's evaluator.  The procedure
  // objects are cached once (glue_vm); s7_is_eq is a pointer compare.
  if (s7_is_eq (f, g_car_fn)) return s7_car (args[0]);
  if (s7_is_eq (f, g_cdr_fn)) return s7_cdr (args[0]);
  if (s7_is_eq (f, g_cons_fn)) return s7_cons (sc, args[0], args[1]);
  if (s7_is_eq (f, g_eq_fn)) return s7_is_eq (args[0], args[1]) ? s7_t (sc) : s7_f (sc);
  if (s7_is_eq (f, g_null_fn)) return s7_is_null (sc, args[0]) ? s7_t (sc) : s7_f (sc);
  if (s7_is_eq (f, g_pair_fn)) return s7_is_pair (args[0]) ? s7_t (sc) : s7_f (sc);
  if (s7_is_eq (f, g_not_fn)) return (args[0] == s7_f (sc)) ? s7_t (sc) : s7_f (sc);
  // Integer fast paths for + and - (the benchmark loops); fall back to s7
  // for non-integer or overflow (s7_apply_function re-checks).
  if (s7_is_eq (f, g_add_fn)) {
    s7_int sum = 0;
    bool ok = !args.empty ();
    for (auto& a : args)
      if (!s7_is_integer (a)) { ok = false; break; }
      else sum += s7_integer (a);
    if (ok) return s7_make_integer (sc, sum);
  }
  if (s7_is_eq (f, g_sub_fn)) {
    if (args.size () == 1) {
      if (s7_is_integer (args[0])) return s7_make_integer (sc, -s7_integer (args[0]));
    } else {
      bool ok = true;
      for (auto& a : args)
        if (!s7_is_integer (a)) { ok = false; break; }
      if (ok) {
        s7_int r = s7_integer (args[0]);
        for (size_t i = 1; i < args.size (); ++i) r -= s7_integer (args[i]);
        return s7_make_integer (sc, r);
      }
    }
  }
  if (s7_is_eq (f, g_num_eq_fn)) {
    if (s7_is_integer (args[0]) && s7_is_integer (args[1]))
      return (s7_integer (args[0]) == s7_integer (args[1])) ? s7_t (sc) : s7_f (sc);
    return s7_apply_function (sc, f, build_args_list (sc, args));
  }
  if (s7_is_eq (f, g_lt_fn)) {
    if (s7_is_integer (args[0]) && s7_is_integer (args[1]))
      return (s7_integer (args[0]) < s7_integer (args[1])) ? s7_t (sc) : s7_f (sc);
    return s7_apply_function (sc, f, build_args_list (sc, args));
  }
  s7_pointer args_list = build_args_list (sc, args);
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
  if (s7_is_eq (f, g_apply_fn)) {
    s7_pointer proc = s7_car (args_list);
    s7_pointer rest = s7_cdr (args_list);            // (a1 ... an)
    if (!s7_is_pair (rest))                          // (apply proc) -- no args
      return s7_apply_function (sc, proc, s7_nil (sc));
    s7_pointer p = rest;
    while (s7_is_pair (s7_cdr (p))) p = s7_cdr (p);  // p is the last cons (an)
    s7_pointer last = s7_car (p);
    s7_pointer spliced;
    if (s7_is_null (sc, last)) {
      if (rest == p)                                 // only (proc ()) -> no args
        spliced = s7_nil (sc);
      else {                                         // drop the () tail
        s7_pointer q = rest;
        while (s7_is_pair (s7_cdr (q)) && s7_cdr (q) != p) q = s7_cdr (q);
        s7_set_cdr (q, s7_nil (sc));
        spliced = rest;
      }
    } else {
      if (rest == p)                                 // only (proc list) -> splice
        spliced = last;
      else {                                         // splice list after fixed args
        s7_pointer q = rest;
        while (s7_is_pair (s7_cdr (q)) && s7_cdr (q) != p) q = s7_cdr (q);
        s7_set_cdr (q, last);
        spliced = rest;
      }
    }
    return s7_apply_function (sc, proc, spliced);
  }
  return s7_apply_function (sc, f, args_list);
}
// run : target-depth -> result
static s7_pointer run (s7_scheme* sc, size_t target_depth) {
  while (g_frames.size () > target_depth) {
    VMFrame& fr = g_frames.back ();
    const std::vector<Instr>& code = *fr.code;
    if (fr.pc >= code.size ()) {
      g_frames.pop_back ();
      if (g_frames.size () <= target_depth) break;
      continue;
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
        push (s7_vector_ref (sc, s7_list_ref (sc, fr.captured, in.b - 1), in.c));
        break;
      case Op::Local:
        push (fr.slots[in.b]);
        break;
      case Op::SetLocal: {
        s7_pointer v = pop ();
        fr.slots[in.b] = v;
        if (fr.shared_slots != nullptr)
          s7_vector_set (sc, fr.shared_slots, in.b, v);
        break;
      }
      case Op::SetRef:
        s7_vector_set (sc, s7_list_ref (sc, fr.captured, in.b - 1), in.c, pop ());
        break;
      case Op::StoreGlobal: {
        s7_pointer v = pop ();
        s7_pointer sym = in.a;
              // Store into the program's resolution env (an inlet such as
        // the-expander-library) when one was given, else the rootlet.
        if (fr.global_env != nullptr && s7_is_let (fr.global_env))
          s7_varlet (sc, fr.global_env, sym, v);
        else
          s7_define_variable (sc, s7_symbol_name (sym), v);
        break;
      }
      case Op::Closure: {
        int i = (int)in.b;
        VMCodeInfo& ci = fr.prog->codes[i];
        VMClosure* vc = new VMClosure;
        vc->prog = fr.prog;
        vc->code_idx = i;
        vc->global_env = fr.global_env;
        // Materialize the shared slot snapshot on first capture.
        if (fr.shared_slots == nullptr) {
          fr.shared_slots = s7_make_vector (sc, (s7_int)fr.slots.size ());
          for (size_t k = 0; k < fr.slots.size (); ++k)
            s7_vector_set (sc, fr.shared_slots, (s7_int)k, fr.slots[k]);
        }
        vc->captured = s7_cons (sc, fr.shared_slots, fr.captured);
        s7_pointer let = s7_inlet (sc,
                                   s7_cons (sc,
                                            s7_cons (sc, captured_symbol, vc->captured),
                                            s7_nil (sc)));
        s7_pointer cobj = s7_make_c_object_with_let (sc, VM_CLOSURE_TYPE, vc, let);
        s7_pointer formals = ci.formals;
        // Build the call formals as a proper list: (x y) -> (x y),
        // (a . rest) -> (a rest), a rest symbol -> (args).  The closure
        // body must be a proper list -- (vm-enter cobj a . rest) cannot be
        // evaluated -- and a rest arg arrives as one list value.
        s7_pointer call_formals;
        if (s7_is_symbol (formals)) {
          call_formals = s7_list (sc, 1, formals);
        } else if (!s7_is_proper_list (sc, formals)) {
          s7_pointer acc = s7_nil (sc);
          s7_pointer f = formals;
          while (s7_is_pair (f)) { acc = s7_cons (sc, s7_car (f), acc); f = s7_cdr (f); }
          acc = s7_cons (sc, f, acc);
          call_formals = s7_reverse (sc, acc);
        } else {
          call_formals = formals;
        }
        s7_pointer call = s7_cons (sc, vm_enter_symbol,
                                   s7_cons (sc, cobj, call_formals));
        s7_pointer body = s7_list (sc, 1, call);
        s7_int arity = s7_is_symbol (formals) ? -1
                                              : s7_list_length (sc, formals);
        push (s7_gf_make_closure (sc, formals, body, arity));
        break;
      }
      case Op::Call:
      case Op::TailCall: {
        int n = (int)in.b;
        std::vector<s7_pointer> args (n);
        for (int i = n - 1; i >= 0; --i) args[i] = pop ();
        s7_pointer f = pop ();
        s7_pointer r;
        if (in.op == Op::TailCall) {
          std::vector<s7_pointer> slots = std::move (fr.slots);
          g_frames.pop_back ();
          r = call_function (sc, f, args, &slots);
        } else {
          r = call_function (sc, f, args, nullptr);
        }
        if (r != nullptr) push (r);
        break;
      }
      case Op::IfElse: {
        s7_pointer t = pop ();
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
        g_frames.pop_back ();
        break;  // if at target_depth the while loop exits and pops the top
      }
      case Op::Pop:
        pop ();
        break;
      case Op::Values: {
        int n = (int)in.b;
        s7_pointer args_list = s7_nil (sc);
        for (int i = 0; i < n; ++i) args_list = s7_cons (sc, pop (), args_list);
        push (s7_values (sc, args_list));
        break;
      }
      case Op::CallWithValues: {
        s7_pointer c = pop ();
        s7_pointer p = pop ();
        size_t d0 = g_frames.size ();
        std::vector<s7_pointer> no_args;
        s7_pointer pr = call_function (sc, p, no_args, nullptr);
        s7_pointer r = (pr != nullptr) ? pr : run (sc, d0);
        std::vector<s7_pointer> c_args;
        if (s7_is_multiple_value (r)) {
          for (s7_pointer a = s7_cdr (r); s7_is_pair (a); a = s7_cdr (a))
            c_args.push_back (s7_car (a));
        } else {
          c_args.push_back (r);
        }
        size_t d1 = g_frames.size ();
        s7_pointer cr = call_function (sc, c, c_args, nullptr);
        if (cr != nullptr) push (cr);
        else push (run (sc, d1));
        break;
      }
      default:
        g_frames.pop_back ();
        return s7_error (sc, s7_make_symbol (sc, "vm-error"),
                         s7_list (sc, 2, s7_make_string (sc, "unknown instruction"),
                                  s7_make_integer (sc, (s7_int)in.op)));
    }
  }
  return g_stack.empty () ? s7_undefined (sc) : pop ();
}

// ---------------------------------------------------------------------------
// vm-load : program env -> top result
// Load a program, set its (global ...) resolution environment (an inlet
// such as the-expander-library, or #f for the rootlet), run the top
// instruction list, and return the value it leaves on the stack (e.g. a
// VM closure).  One program at a time: loading replaces the previous
// program and invalidates closures built from it.
static s7_pointer vm_load (s7_scheme* sc, s7_pointer args) {
  s7_pointer program = s7_car (args);
  s7_gc_protect (sc, program);
  s7_pointer global_env = s7_cadr (args);
  VMProgram* p = new VMProgram;
  s7_pointer ctable = s7_cadr (program);
  for (s7_pointer c = s7_cdr (ctable); s7_is_pair (c); c = s7_cdr (c)) {
    s7_pointer code = s7_car (c);
    VMCodeInfo ci;
    ci.nlocals = s7_integer (s7_cadr (code));
    ci.formals = s7_caddr (code);
    ci.instrs = decode_instrs (sc, s7_cadddr (code));
    p->codes.push_back (ci);
  }
  p->top = decode_instrs (sc, s7_cdr (s7_caddr (program)));
  g_program = p;
  g_stack.clear ();
  g_frames.clear ();
  VMFrame f;
  f.pc = 0;
  f.code = &p->top;
  f.prog = p;
  f.global_env = global_env;
  f.slots.clear ();
  f.shared_slots = nullptr;
  f.captured = s7_nil (sc);
  g_frames.push_back (f);
  // The interpreter's C++ stacks hold s7_pointers the conservative GC cannot
  // see (same reason vm_enter disables it): running the top instructions with
  // GC enabled can reclaim live values, leaving dangling pointers that later
  // corrupt the heap.  Disable GC for the run, exactly like vm_enter.
  bool saved_gc = s7_gc_enabled (sc);
  s7_gc_on (sc, false);
  s7_pointer result = run (sc, 0);
  s7_gc_on (sc, saved_gc);
  return result;
}

// ---------------------------------------------------------------------------
// vm-enter : (cobj . args) -> result
static s7_pointer vm_enter (s7_scheme* sc, s7_pointer args) {
  s7_pointer cobj = s7_car (args);
  VMClosure* vc = static_cast<VMClosure*>(s7_c_object_value (cobj));
  bool saved_gc = s7_gc_enabled (sc);
  s7_gc_on (sc, false);
  // vm-enter can be called NESTED inside another VM run (a VM program calling
  // into s7, which calls back a VM closure): run must stop at the depth that
  // existed before we pushed, not 0, or it pops the enclosing frame too.
  size_t d0 = g_frames.size ();
  std::vector<s7_pointer> arg_list;
  for (s7_pointer a = s7_cdr (args); s7_is_pair (a); a = s7_cdr (a))
    arg_list.push_back (s7_car (a));
  push_frame (sc, vc->prog, vc->code_idx, arg_list, vc->captured, vc->global_env, nullptr);
  s7_pointer result = run (sc, d0);
  s7_gc_on (sc, saved_gc);
  return result;
}

// ---------------------------------------------------------------------------

void glue_vm (s7_scheme* sc) {
  VM_CLOSURE_TYPE = s7_make_c_type (sc, "vm-closure");
  g_apply_fn = s7_gf_global_value (sc, s7_make_symbol (sc, "apply"));
  g_car_fn   = s7_gf_global_value (sc, s7_make_symbol (sc, "car"));
  g_cdr_fn   = s7_gf_global_value (sc, s7_make_symbol (sc, "cdr"));
  g_cons_fn  = s7_gf_global_value (sc, s7_make_symbol (sc, "cons"));
  g_eq_fn    = s7_gf_global_value (sc, s7_make_symbol (sc, "eq?"));
  g_null_fn  = s7_gf_global_value (sc, s7_make_symbol (sc, "null?"));
  g_pair_fn  = s7_gf_global_value (sc, s7_make_symbol (sc, "pair?"));
  g_not_fn   = s7_gf_global_value (sc, s7_make_symbol (sc, "not"));
  g_add_fn   = s7_gf_global_value (sc, s7_make_symbol (sc, "+"));
  g_sub_fn   = s7_gf_global_value (sc, s7_make_symbol (sc, "-"));
  g_num_eq_fn = s7_gf_global_value (sc, s7_make_symbol (sc, "="));
  g_lt_fn    = s7_gf_global_value (sc, s7_make_symbol (sc, "<"));
  g_false = s7_f (sc);
  vm_enter_symbol  = s7_make_symbol (sc, "vm-enter");
  quote_symbol     = s7_make_symbol (sc, "quote");
  captured_symbol  = s7_make_symbol (sc, "*vm-captured*");
  vm_const_symbol  = s7_make_symbol (sc, "const");
  vm_global_symbol = s7_make_symbol (sc, "global");
  vm_ref_symbol    = s7_make_symbol (sc, "ref");
  vm_local_symbol  = s7_make_symbol (sc, "local");
  vm_set_local_symbol = s7_make_symbol (sc, "set-local");
  vm_set_ref_symbol = s7_make_symbol (sc, "set-ref");
  vm_store_global_symbol = s7_make_symbol (sc, "store-global");
  vm_closure_symbol = s7_make_symbol (sc, "closure");
  vm_call_symbol   = s7_make_symbol (sc, "call");
  vm_tail_call_symbol = s7_make_symbol (sc, "tail-call");
  vm_if_else_symbol = s7_make_symbol (sc, "if-else");
  vm_jump_symbol   = s7_make_symbol (sc, "jump");
  vm_label_symbol  = s7_make_symbol (sc, "label");
  vm_return_symbol = s7_make_symbol (sc, "return");
  vm_pop_symbol    = s7_make_symbol (sc, "pop");
  vm_values_symbol = s7_make_symbol (sc, "values");
  vm_call_with_values_symbol = s7_make_symbol (sc, "call-with-values");

  s7_define_function (sc, "vm-load", vm_load, 2, 0, false,
                      "(vm-load program global-env)");
  s7_define_function (sc, "vm-enter", vm_enter, 1, 0, true, "(vm-enter cobj . args)");
}

} // namespace goldfish
