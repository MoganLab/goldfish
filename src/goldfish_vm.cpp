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

#include "gf.h"
#include <deque>
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
// A deque, not a vector: a nested VM call (VM closure invoked from s7,
// which calls back into the VM, e.g. map over a VM closure) pushes frames
// from inside the run loop; a vector realloc would invalidate the loop's
// `VMFrame& fr = g_frames.back()` reference, leaving it dangling.
static std::deque<VMFrame> g_frames;
static s7_pointer g_apply_fn = nullptr;  // the rootlet 'apply procedure
static s7_pointer g_car_fn = nullptr, g_cdr_fn = nullptr, g_cons_fn = nullptr;
static s7_pointer g_eq_fn = nullptr, g_null_fn = nullptr, g_pair_fn = nullptr;
static s7_pointer g_not_fn = nullptr, g_add_fn = nullptr, g_sub_fn = nullptr;
static s7_pointer g_num_eq_fn = nullptr, g_lt_fn = nullptr;
static s7_pointer g_map_fn = nullptr;

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
  if (env != nullptr && gf::is_let (env)) {
    s7_pointer v = gf::let_ref (sc, env, name);
    if (v != gf::undefined (sc)) return v;
  }
  return gf::global_value (sc, name);
}

// ---------------------------------------------------------------------------
// Instruction decoding.

static std::vector<Instr> decode_instrs (s7_scheme* sc, s7_pointer instr_list) {
  std::vector<Instr> v;
  for (s7_pointer p = instr_list; gf::is_pair (p); p = gf::cdr (p)) {
    s7_pointer instr = gf::car (p);
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
  f.slots.assign (ci.nlocals, gf::nil (sc));
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
         ? gf::nil (sc)
         : gf::array_to_list (sc, (s7_int)args.size (),
                             const_cast<s7_pointer*>(args.data ()));
}

// call_function : f (list arg) reuse-slots -> result or nullptr
// A VM function pushes a frame and returns nullptr (the loop continues);
// anything else is called with s7_call and its result returned.  reuse
// (optional) lets a tail call hand its slot array to the new frame.
static s7_pointer call_function (s7_scheme* sc, s7_pointer f, const std::vector<s7_pointer>& args, std::vector<s7_pointer>* reuse) {
  if (gf::is_closure (f)) {
    s7_pointer body = gf::closure_body (sc, f);
    if (gf::is_pair (body) && gf::is_pair (gf::car (body)) &&
        gf::is_eq (gf::car (gf::car (body)), vm_enter_symbol)) {
      s7_pointer cobj = gf::cadr (gf::car (body));
      VMClosure* vc = static_cast<VMClosure*>(gf::c_object_value (cobj));
      VMCodeInfo& ci = vc->prog->codes[vc->code_idx];
      if (gf::is_symbol (ci.formals)) {
        // Rest closure: pack ALL arguments into a single list argument.
        std::vector<s7_pointer> packed (1);
        packed[0] = build_args_list (sc, args);
        push_frame (sc, vc->prog, vc->code_idx, packed, vc->captured, vc->global_env, reuse);
      } else if (!gf::is_proper_list (sc, ci.formals)) {
        // Dotted formals (fixed . rest): fixed params then one list for the rest.
        size_t fixed = 0;
        for (s7_pointer p = ci.formals; gf::is_pair (p); p = gf::cdr (p))
          ++fixed;
        std::vector<s7_pointer> packed (fixed + 1);
        for (size_t i = 0; i < fixed; ++i)
          packed[i] = (i < args.size ()) ? args[i] : gf::nil (sc);
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
    s7_int sum = 0;
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
        s7_int r = gf::integer (args[0]);
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
  if (gf::is_eq (f, g_apply_fn)) {
    s7_pointer proc = gf::car (args_list);
    s7_pointer rest = gf::cdr (args_list);            // (a1 ... an)
    if (!gf::is_pair (rest))                          // (apply proc) -- no args
      return gf::apply_function (sc, proc, gf::nil (sc));
    s7_pointer p = rest;
    while (gf::is_pair (gf::cdr (p))) p = gf::cdr (p);  // p is the last cons (an)
    s7_pointer last = gf::car (p);
    s7_pointer spliced;
    if (gf::is_null (sc, last)) {
      if (rest == p)                                 // only (proc ()) -> no args
        spliced = gf::nil (sc);
      else {                                         // drop the () tail
        s7_pointer q = rest;
        while (gf::is_pair (gf::cdr (q)) && gf::cdr (q) != p) q = gf::cdr (q);
        gf::set_cdr (q, gf::nil (sc));
        spliced = rest;
      }
    } else {
      if (rest == p)                                 // only (proc list) -> splice
        spliced = last;
      else {                                         // splice list after fixed args
        s7_pointer q = rest;
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
        push (gf::vector_ref (sc, gf::list_ref (sc, fr.captured, in.b - 1), in.c));
        break;
      case Op::Local: {
        // Top-level expressions (e.g. a library registration let) allocate
        // slots on a frame with no fixed nlocals; grow on demand.
        if ((s7_int)fr.slots.size () <= in.b)
          fr.slots.resize (in.b + 1, gf::nil (sc));
        push (fr.slots[in.b]);
        break;
      }
      case Op::SetLocal: {
        s7_pointer v = pop ();
        if ((s7_int)fr.slots.size () <= in.b)
          fr.slots.resize (in.b + 1, gf::nil (sc));
        fr.slots[in.b] = v;
        if (fr.shared_slots != nullptr) {
          if (gf::vector_length (fr.shared_slots) < (s7_int)fr.slots.size ()) {
            // Frame grew on demand (top-level expression): rebuild the
            // shared snapshot so closure captures see all slots.
            s7_pointer ns = gf::make_vector (sc, (s7_int)fr.slots.size ());
            for (size_t k = 0; k < fr.slots.size (); ++k)
              gf::vector_set (sc, ns, (s7_int)k, fr.slots[k]);
            fr.shared_slots = ns;
          } else {
            gf::vector_set (sc, fr.shared_slots, in.b, v);
          }
        }
        break;
      }
      case Op::SetRef:
        gf::vector_set (sc, gf::list_ref (sc, fr.captured, in.b - 1), in.c, pop ());
        break;
      case Op::StoreGlobal: {
        s7_pointer v = pop ();
        s7_pointer sym = in.a;
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
        // Materialize the shared slot snapshot on first capture.
        if (fr.shared_slots == nullptr) {
          fr.shared_slots = gf::make_vector (sc, (s7_int)fr.slots.size ());
          for (size_t k = 0; k < fr.slots.size (); ++k)
            gf::vector_set (sc, fr.shared_slots, (s7_int)k, fr.slots[k]);
        }
        vc->captured = gf::cons (sc, fr.shared_slots, fr.captured);
        s7_pointer let = gf::inlet (sc,
                                   gf::cons (sc,
                                            gf::cons (sc, captured_symbol, vc->captured),
                                            gf::nil (sc)));
        s7_pointer cobj = gf::make_c_object_with_let (sc, VM_CLOSURE_TYPE, vc, let);
        s7_pointer formals = ci.formals;
        // Build the call formals as a proper list: (x y) -> (x y),
        // (a . rest) -> (a rest), a rest symbol -> (args).  The closure
        // body must be a proper list -- (vm-enter cobj a . rest) cannot be
        // evaluated -- and a rest arg arrives as one list value.
        s7_pointer call_formals;
        if (gf::is_symbol (formals)) {
          call_formals = gf::list (sc, formals);
        } else if (!gf::is_proper_list (sc, formals)) {
          s7_pointer acc = gf::nil (sc);
          s7_pointer f = formals;
          while (gf::is_pair (f)) { acc = gf::cons (sc, gf::car (f), acc); f = gf::cdr (f); }
          acc = gf::cons (sc, f, acc);
          call_formals = gf::reverse (sc, acc);
        } else {
          call_formals = formals;
        }
        s7_pointer call = gf::cons (sc, vm_enter_symbol,
                                   gf::cons (sc, cobj, call_formals));
        s7_pointer body = gf::list (sc, call);
        s7_int arity = gf::is_symbol (formals) ? -1
                                              : gf::list_length (sc, formals);
        push (gf::make_closure (sc, formals, body, arity));
        break;
      }
      case Op::Call:
      case Op::TailCall: {
        int n = (int)in.b;
        std::vector<s7_pointer> args (n);
        for (int i = n - 1; i >= 0; --i) args[i] = pop ();
        s7_pointer f = pop ();
        s7_pointer r;
        // Fast path for (map proc seq): s7's map defers a one-expression
        // closure body it cannot cell-optimize (a VM closure shell is
        // (lambda x (vm-enter ...)) -- vm-enter is a C function, so the
        // optimizer gives up) onto OP_MAP_2 and returns unspecified; the
        // collected values then never reach the VM.  Walk the sequence
        // directly so the VM closure callback runs through call_function.
        if (gf::is_eq (f, g_map_fn) && args.size () == 2) {
          size_t d0 = g_frames.size ();
          std::vector<s7_pointer> vals;
          for (s7_pointer p = args[1]; gf::is_pair (p); p = gf::cdr (p)) {
            std::vector<s7_pointer> a (1);
            a[0] = gf::car (p);
            s7_pointer v = call_function (sc, args[0], a, nullptr);
            if (v == nullptr) v = run (sc, d0);
            vals.push_back (v);
          }
          s7_pointer lst = gf::nil (sc);
          for (size_t i = vals.size (); i > 0; --i)
            lst = gf::cons (sc, vals[i - 1], lst);
          r = lst;
        } else if (in.op == Op::TailCall) {
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
        s7_pointer args_list = gf::nil (sc);
        for (int i = 0; i < n; ++i) args_list = gf::cons (sc, pop (), args_list);
        push (gf::values (sc, args_list));
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
        if (gf::is_multiple_value (r)) {
          for (s7_pointer a = gf::cdr (r); gf::is_pair (a); a = gf::cdr (a))
            c_args.push_back (gf::car (a));
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
        return gf::error (sc, gf::make_symbol (sc, "vm-error"),
                         gf::list (sc, gf::make_string (sc, "unknown instruction"),
                                  gf::make_integer (sc, (s7_int)in.op)));
    }
  }
  return g_stack.empty () ? gf::undefined (sc) : pop ();
}

// ---------------------------------------------------------------------------
// vm-load : program env -> top result
// Load a program, set its (global ...) resolution environment (an inlet
// such as the-expander-library, or #f for the rootlet), run the top
// instruction list, and return the value it leaves on the stack (e.g. a
// VM closure).  One program at a time: loading replaces the previous
// program and invalidates closures built from it.
static s7_pointer vm_load (s7_scheme* sc, s7_pointer args) {
  s7_pointer program = gf::car (args);
  gf::gc_protect (sc, program);
  s7_pointer global_env = gf::cadr (args);
  VMProgram* p = new VMProgram;
  s7_pointer ctable = gf::cadr (program);
  for (s7_pointer c = gf::cdr (ctable); gf::is_pair (c); c = gf::cdr (c)) {
    s7_pointer code = gf::car (c);
    VMCodeInfo ci;
    ci.nlocals = gf::integer (gf::cadr (code));
    ci.formals = gf::caddr (code);
    ci.instrs = decode_instrs (sc, gf::cadddr (code));
    p->codes.push_back (ci);
  }
  s7_pointer top = gf::caddr (program);
  s7_int top_nlocals = 0;
  if (gf::is_pair (gf::cdr (top)) && gf::is_integer (gf::cadr (top))) {
    // (top <nlocals> instr...) -- slot count for top-level expressions
    // (a top-level let's bindings are captured by lambdas).
    top_nlocals = gf::integer (gf::cadr (top));
    p->top = decode_instrs (sc, gf::cddr (top));
  } else {
    // Legacy (top instr...): no top-level slots.
    p->top = decode_instrs (sc, gf::cdr (top));
  }
  g_program = p;
  g_stack.clear ();
  g_frames.clear ();
  VMFrame f;
  f.pc = 0;
  f.code = &p->top;
  f.prog = p;
  f.global_env = global_env;
  f.slots.assign ((size_t)top_nlocals, gf::nil (sc));
  f.shared_slots = nullptr;
  f.captured = gf::nil (sc);
  g_frames.push_back (f);
  // The interpreter's C++ stacks hold s7_pointers the conservative GC cannot
  // see (same reason vm_enter disables it): running the top instructions with
  // GC enabled can reclaim live values, leaving dangling pointers that later
  // corrupt the heap.  Disable GC for the run, exactly like vm_enter.
  bool saved_gc = gf::gc_enabled (sc);
  gf::gc_on (sc, false);
  s7_pointer result = run (sc, 0);
  gf::gc_on (sc, saved_gc);
  return result;
}

// ---------------------------------------------------------------------------
// vm-enter : (cobj . args) -> result
static s7_pointer vm_enter (s7_scheme* sc, s7_pointer args) {
  s7_pointer cobj = gf::car (args);
  VMClosure* vc = static_cast<VMClosure*>(gf::c_object_value (cobj));
  bool saved_gc = gf::gc_enabled (sc);
  gf::gc_on (sc, false);
  // vm-enter can be called NESTED inside another VM run (a VM program calling
  // into s7, which calls back a VM closure): run must stop at the depth that
  // existed before we pushed, not 0, or it pops the enclosing frame too.
  size_t d0 = g_frames.size ();
  std::vector<s7_pointer> arg_list;
  for (s7_pointer a = gf::cdr (args); gf::is_pair (a); a = gf::cdr (a))
    arg_list.push_back (gf::car (a));
  push_frame (sc, vc->prog, vc->code_idx, arg_list, vc->captured, vc->global_env, nullptr);
  s7_pointer result = run (sc, d0);
  gf::gc_on (sc, saved_gc);
  return result;
}

// ---------------------------------------------------------------------------

void glue_vm (s7_scheme* sc) {
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
  g_map_fn   = gf::global_value (sc, gf::make_symbol (sc, "map"));
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
