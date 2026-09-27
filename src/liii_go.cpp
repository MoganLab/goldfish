//
// Copyright (C) 2026 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "liii_go.hpp"
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <queue>
#include <sstream>
#include <thread>
#include <unordered_map>

namespace goldfish {

void glue_for_community_edition (s7_scheme* sc);

static std::string g_goldfish_lib_directory;
static std::mutex  g_lib_dir_mtx;

void
set_goldfish_lib_dir (const std::string& dir) {
  std::lock_guard<std::mutex> lock (g_lib_dir_mtx);
  g_goldfish_lib_directory= dir;
}

std::string
get_goldfish_lib_dir () {
  std::lock_guard<std::mutex> lock (g_lib_dir_mtx);
  return g_goldfish_lib_directory;
}

static s7_pointer
f_worker_notify_error (s7_scheme* sc, s7_pointer args) {
  char* tag_str= s7_object_to_c_string (sc, s7_car (args));
  char* err_str= s7_object_to_c_string (sc, s7_cadr (args));
  std::cerr << "[Goldfish Worker Error] " << (tag_str ? tag_str : "?") << ": " << (err_str ? err_str : "?")
            << std::endl;
  free (tag_str);
  free (err_str);
  return s7_unspecified (sc);
}

// ---------------------------------------------------------------------------
// Go Task and Thread Pool
// ---------------------------------------------------------------------------

struct GoTask {
  std::vector<std::string> var_names;
  std::vector<GFValue>     var_vals;
  GFValue                  code_expr;
};

static size_t
configured_worker_count () {
  static const size_t n= [] () {
    size_t c= std::thread::hardware_concurrency ();
    return c == 0 ? 4 : c;
  }();
  return n;
}

class GoThreadPool {
public:
  static GoThreadPool& instance () {
    static GoThreadPool pool;
    return pool;
  }

  void enqueue (GoTask task) {
    {
      std::unique_lock<std::mutex> lock (mtx);
      tasks.push (std::move (task));
    }
    cv.notify_one ();
  }

  void shutdown () {
    {
      std::unique_lock<std::mutex> lock (mtx);
      if (stop) return;
      stop= true;
    }
    cv.notify_all ();
    for (auto& w : workers) {
      if (w.joinable ()) {
        w.join ();
      }
    }
    workers.clear ();
  }

private:
  GoThreadPool () {
    size_t n= configured_worker_count ();
    for (size_t i= 0; i < n; ++i) {
      workers.emplace_back ([this] () { worker_loop (); });
    }
  }

  ~GoThreadPool () { shutdown (); }

  void worker_loop () {
    s7_scheme*  worker_sc= s7_init ();
    std::string lib_dir  = get_goldfish_lib_dir ();
    if (!lib_dir.empty ()) {
      s7_add_to_load_path (worker_sc, lib_dir.c_str ());
    }
    glue_for_community_edition (worker_sc);
    s7_eval_c_string (worker_sc, "(import (scheme base) (scheme time) (liii base) (liii go))");
    // 错误处理闭包定义一次并锚定在 rootlet，避免每个任务重建及被 GC 回收
    s7_eval_c_string (worker_sc,
                      "(define *go-err-handler* (lambda (err-tag err-args) (g_worker-notify-error err-tag err-args)))");

    // 以下符号均被 rootlet 常驻引用，跨任务复用是 GC 安全的
    s7_pointer lambda_sym = s7_make_symbol (worker_sc, "lambda");
    s7_pointer catch_sym  = s7_make_symbol (worker_sc, "catch");
    s7_pointer let_sym    = s7_make_symbol (worker_sc, "let");
    s7_pointer handler_sym= s7_make_symbol (worker_sc, "*go-err-handler*");

    while (true) {
      GoTask task;
      {
        std::unique_lock<std::mutex> lock (mtx);
        cv.wait (lock, [this] () { return stop || !tasks.empty (); });
        if (stop && tasks.empty ()) break;
        task= std::move (tasks.front ());
        tasks.pop ();
      }

      // Build bindings: ((name1 val1) (name2 val2) ...)
      s7_pointer bindings= s7_nil (worker_sc);
      for (size_t i= task.var_names.size (); i > 0; --i) {
        size_t     idx = i - 1;
        s7_pointer sym = s7_make_symbol (worker_sc, task.var_names[idx].c_str ());
        s7_pointer val = gfvalue_to_s7 (worker_sc, task.var_vals[idx]);
        s7_pointer pair= s7_list (worker_sc, 2, sym, val);
        bindings       = s7_cons (worker_sc, pair, bindings);
      }

      // (let bindings (catch #t (lambda () body) *go-err-handler*))
      s7_pointer body      = gfvalue_to_s7 (worker_sc, task.code_expr);
      s7_pointer body_thunk= s7_list (worker_sc, 3, lambda_sym, s7_nil (worker_sc), body);
      s7_pointer catch_expr= s7_list (worker_sc, 4, catch_sym, s7_t (worker_sc), body_thunk, handler_sym);
      s7_pointer let_expr  = s7_cons (worker_sc, let_sym,
                                      s7_cons (worker_sc, bindings, s7_cons (worker_sc, catch_expr, s7_nil (worker_sc))));

      s7_eval (worker_sc, let_expr, s7_rootlet (worker_sc));
    }
  }

  std::vector<std::thread> workers;
  std::queue<GoTask>       tasks;
  std::mutex               mtx;
  std::condition_variable  cv;
  bool                     stop= false;
};

// ---------------------------------------------------------------------------
// Timer Facility：单一定时线程 + 最小堆，到点关闭 channel
// 避免每个 timeout context 独占一个 worker 线程导致线程池饿死
// ---------------------------------------------------------------------------

class GoTimer {
public:
  static GoTimer& instance () {
    static GoTimer timer;
    return timer;
  }

  void schedule_close (std::shared_ptr<GoldfishChannel> ch, int64_t delay_ms) {
    {
      std::lock_guard<std::mutex> lock (mtx);
      entries.push ({std::chrono::steady_clock::now () + std::chrono::milliseconds (delay_ms), std::move (ch)});
    }
    cv.notify_one ();
  }

private:
  struct Entry {
    std::chrono::steady_clock::time_point deadline;
    std::shared_ptr<GoldfishChannel>    ch;
    bool operator> (const Entry& o) const { return deadline > o.deadline; }
  };

  GoTimer () : worker ([this] () { loop (); }) {}

  ~GoTimer () {
    {
      std::lock_guard<std::mutex> lock (mtx);
      stop= true;
    }
    cv.notify_one ();
    if (worker.joinable ()) worker.join ();
  }

  void loop () {
    std::unique_lock<std::mutex> lock (mtx);
    while (!stop) {
      if (entries.empty ()) {
        cv.wait (lock);
        continue;
      }
      if (entries.top ().deadline <= std::chrono::steady_clock::now ()) {
        auto ch= entries.top ().ch;
        entries.pop ();
        lock.unlock (); // 关闭 channel 会唤醒其等待者，不持有定时器锁执行
        ch->close ();
        lock.lock ();
      }
      else {
        cv.wait_until (lock, entries.top ().deadline);
      }
    }
  }

  std::priority_queue<Entry, std::vector<Entry>, std::greater<Entry>> entries;
  std::mutex                                                          mtx;
  std::condition_variable                                             cv;
  std::thread                                                         worker;
  bool                                                                stop= false;
};

static std::mutex                             g_type_mtx;
static std::unordered_map<s7_scheme*, s7_int> g_channel_type_tags;

// 序列化错误消息缓冲（TLS，保证 go_error 抛出时该字符串仍然存活）
static thread_local std::string t_serialize_err;

// 重要约束：go_error 底层是 s7_error 的 longjmp，会直接跳过 C++ 栈帧上
// RAII 对象的析构。Linux 下这只是泄漏，但 MSVC 的 longjmp 跳过非平凡析构
// 对象会导致进程崩溃（已在 Windows CI 上以 access violation 复现）。
// 因此本文件中所有 go_error 调用点必须保证当前函数帧内没有存活的 RAII
// 对象：先用平凡局部变量做参数检查，RAII 操作收进内层作用域或辅助函数，
// 错误在 RAII 对象析构之后抛出。
static s7_pointer
go_error (s7_scheme* sc, const char* kind, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, kind), s7_list (sc, 2, s7_make_string (sc, msg), arg));
}

static void
channel_free_c_value (void* val) {
  if (val != nullptr) {
    auto* ch_ptr= static_cast<std::shared_ptr<GoldfishChannel>*> (val);
    delete ch_ptr;
  }
}

static s7_pointer
channel_to_string_glue (s7_scheme* sc, s7_pointer args) {
  s7_pointer         self  = s7_car (args);
  auto*              ch_ptr= static_cast<std::shared_ptr<GoldfishChannel>*> (s7_c_object_value (self));
  std::ostringstream oss;
  oss << "#<channel " << ch_ptr->get () << " cap=" << (*ch_ptr)->get_capacity () << ">";
  return s7_make_string (sc, oss.str ().c_str ());
}

static s7_pointer
channel_is_equal_glue (s7_scheme* sc, s7_pointer args) {
  s7_pointer a= s7_car (args);
  s7_pointer b= s7_cadr (args);
  if (!is_goldfish_channel (sc, a) || !is_goldfish_channel (sc, b)) {
    return s7_f (sc);
  }
  auto* ch_a= static_cast<std::shared_ptr<GoldfishChannel>*> (s7_c_object_value (a));
  auto* ch_b= static_cast<std::shared_ptr<GoldfishChannel>*> (s7_c_object_value (b));
  return s7_make_boolean (sc, ch_a->get () == ch_b->get ());
}

bool
is_goldfish_channel (s7_scheme* sc, s7_pointer obj) {
  if (!s7_is_c_object (obj)) return false;
  std::lock_guard<std::mutex> lock (g_type_mtx);
  auto                        it= g_channel_type_tags.find (sc);
  if (it == g_channel_type_tags.end ()) return false;
  return s7_c_object_type (obj) == it->second;
}

std::shared_ptr<GoldfishChannel>
get_goldfish_channel (s7_scheme* sc, s7_pointer obj) {
  if (!is_goldfish_channel (sc, obj)) return nullptr;
  auto* ch_ptr= static_cast<std::shared_ptr<GoldfishChannel>*> (s7_c_object_value (obj));
  return *ch_ptr;
}

s7_pointer
make_goldfish_channel_object (s7_scheme* sc, std::shared_ptr<GoldfishChannel> ch) {
  s7_int tag= 0;
  {
    std::lock_guard<std::mutex> lock (g_type_mtx);
    auto                        it= g_channel_type_tags.find (sc);
    if (it == g_channel_type_tags.end ()) {
      return s7_f (sc);
    }
    tag= it->second;
  }
  auto* p= new std::shared_ptr<GoldfishChannel> (std::move (ch));
  return s7_make_c_object (sc, tag, p);
}

// ---------------------------------------------------------------------------
// GFValue Serialization & Deserialization
// ---------------------------------------------------------------------------

struct SerializeCtx {
  std::unordered_map<void*, size_t> visited;
  size_t                            next_id= 1;
};

struct DeserializeCtx {
  std::unordered_map<size_t, s7_pointer> reconstructed;
  s7_pointer                             gc_anchor= nullptr;
  s7_int                                 gc_loc   = -1;
};

static bool
s7_to_gfvalue_impl (s7_scheme* sc, s7_pointer obj, GFValue& out, std::string& err_msg, SerializeCtx& ctx) {
  if (s7_is_null (sc, obj)) {
    out.type= GFValueType::Nil;
    return true;
  }
  if (s7_is_boolean (obj)) {
    out.type    = GFValueType::Boolean;
    out.bool_val= (obj == s7_t (sc));
    return true;
  }
  if (s7_is_integer (obj)) {
    out.type   = GFValueType::Integer;
    out.int_val= s7_integer (obj);
    return true;
  }
  if (s7_is_real (obj)) {
    out.type      = GFValueType::Real;
    out.double_val= s7_real (obj);
    return true;
  }
  if (s7_is_character (obj)) {
    out.type    = GFValueType::Character;
    out.char_val= static_cast<uint32_t> (s7_character (obj));
    return true;
  }
  if (s7_is_string (obj)) {
    out.type   = GFValueType::String;
    out.str_val= std::string (s7_string (obj), s7_string_length (obj));
    return true;
  }
  if (s7_is_symbol (obj)) {
    out.type   = GFValueType::Symbol;
    out.str_val= s7_symbol_name (obj);
    return true;
  }
  if (s7_is_syntax (obj) || s7_is_procedure (obj)) {
    char* str= s7_object_to_c_string (sc, obj);
    if (str != nullptr && std::strncmp (str, "#_", 2) == 0) {
      out.type   = GFValueType::Symbol;
      out.str_val= str + 2;
      free (str);
      return true;
    }
    free (str);
  }
  if (is_goldfish_channel (sc, obj)) {
    out.type    = GFValueType::Channel;
    out.chan_val= get_goldfish_channel (sc, obj);
    return true;
  }
  if (obj == s7_eof_object (sc)) {
    out.type= GFValueType::Eof;
    return true;
  }

  // Composite structures: Check cycle/memoization
  void* ptr= reinterpret_cast<void*> (obj);
  auto  it = ctx.visited.find (ptr);
  if (it != ctx.visited.end ()) {
    out.type  = GFValueType::Ref;
    out.ref_id= it->second;
    return true;
  }

  size_t my_id    = ctx.next_id++;
  ctx.visited[ptr]= my_id;
  out.node_id     = my_id;

  if (s7_is_pair (obj)) {
    out.type     = GFValueType::Pair;
    auto pair_ptr= std::make_shared<std::pair<GFValue, GFValue>> ();
    if (!s7_to_gfvalue_impl (sc, s7_car (obj), pair_ptr->first, err_msg, ctx)) return false;
    if (!s7_to_gfvalue_impl (sc, s7_cdr (obj), pair_ptr->second, err_msg, ctx)) return false;
    out.pair_val= pair_ptr;
    return true;
  }
  if (s7_is_byte_vector (obj)) {
    out.type            = GFValueType::ByteVector;
    s7_int         len  = s7_vector_length (obj);
    const uint8_t* bytes= reinterpret_cast<const uint8_t*> (s7_byte_vector_elements (obj));
    out.bytevec_val     = std::make_shared<const std::vector<uint8_t>> (bytes, bytes + len);
    return true;
  }
  if (s7_is_vector (obj)) {
    out.type      = GFValueType::Vector;
    s7_int len    = s7_vector_length (obj);
    auto   vec_ptr= std::make_shared<std::vector<GFValue>> ();
    vec_ptr->resize (len);
    for (s7_int i= 0; i < len; ++i) {
      s7_pointer elem= s7_vector_ref (sc, obj, i);
      if (!s7_to_gfvalue_impl (sc, elem, (*vec_ptr)[i], err_msg, ctx)) return false;
    }
    out.vec_val= vec_ptr;
    return true;
  }
  if (s7_is_let (obj)) {
    out.type           = GFValueType::Let;
    s7_pointer alist   = s7_let_to_list (sc, obj);
    auto       pair_ptr= std::make_shared<std::pair<GFValue, GFValue>> ();
    if (!s7_to_gfvalue_impl (sc, alist, pair_ptr->first, err_msg, ctx)) return false;
    out.pair_val= pair_ptr;
    return true;
  }

  char* obj_str= s7_object_to_c_string (sc, obj);
  err_msg      = std::string ("unsupported object type for channel serialization: ") + (obj_str ? obj_str : "?");
  free (obj_str);
  return false;
}

bool
s7_to_gfvalue (s7_scheme* sc, s7_pointer obj, GFValue& out, std::string& err_msg) {
  SerializeCtx ctx;
  return s7_to_gfvalue_impl (sc, obj, out, err_msg, ctx);
}

static s7_pointer
gfvalue_to_s7_impl (s7_scheme* sc, const GFValue& val, DeserializeCtx& ctx) {
  if (val.type == GFValueType::Ref) {
    auto it= ctx.reconstructed.find (val.ref_id);
    if (it != ctx.reconstructed.end ()) {
      return it->second;
    }
    return s7_nil (sc);
  }

  switch (val.type) {
  case GFValueType::Nil:
    return s7_nil (sc);
  case GFValueType::Boolean:
    return s7_make_boolean (sc, val.bool_val);
  case GFValueType::Integer:
    return s7_make_integer (sc, val.int_val);
  case GFValueType::Real:
    return s7_make_real (sc, val.double_val);
  case GFValueType::Character:
    return s7_make_character (sc, val.char_val);
  case GFValueType::String:
    return s7_make_string_with_length (sc, val.str_val.data (), val.str_val.size ());
  case GFValueType::Symbol:
    return s7_make_symbol (sc, val.str_val.c_str ());
  case GFValueType::Channel:
    return make_goldfish_channel_object (sc, val.chan_val);
  case GFValueType::Pair: {
    s7_pointer cell= s7_cons (sc, s7_nil (sc), s7_nil (sc));
    if (ctx.gc_loc >= 0) {
      ctx.gc_anchor= s7_cons (sc, cell, ctx.gc_anchor);
      s7_gc_protect_via_location (sc, ctx.gc_anchor, ctx.gc_loc);
    }
    if (val.node_id != 0) {
      ctx.reconstructed[val.node_id]= cell;
    }
    if (val.pair_val) {
      s7_pointer car_p= gfvalue_to_s7_impl (sc, val.pair_val->first, ctx);
      s7_pointer cdr_p= gfvalue_to_s7_impl (sc, val.pair_val->second, ctx);
      s7_set_car (cell, car_p);
      s7_set_cdr (cell, cdr_p);
    }
    return cell;
  }
  case GFValueType::Vector: {
    s7_int     len= val.vec_val ? static_cast<s7_int> (val.vec_val->size ()) : 0;
    s7_pointer vec= s7_make_vector (sc, len);
    if (ctx.gc_loc >= 0) {
      ctx.gc_anchor= s7_cons (sc, vec, ctx.gc_anchor);
      s7_gc_protect_via_location (sc, ctx.gc_anchor, ctx.gc_loc);
    }
    if (val.node_id != 0) {
      ctx.reconstructed[val.node_id]= vec;
    }
    if (val.vec_val) {
      for (s7_int i= 0; i < len; ++i) {
        s7_vector_set (sc, vec, i, gfvalue_to_s7_impl (sc, (*val.vec_val)[i], ctx));
      }
    }
    return vec;
  }
  case GFValueType::ByteVector: {
    if (!val.bytevec_val) return s7_make_byte_vector (sc, 0, 1, nullptr);
    s7_int     len= static_cast<s7_int> (val.bytevec_val->size ());
    s7_pointer bv = s7_make_byte_vector (sc, len, 1, nullptr);
    if (ctx.gc_loc >= 0) {
      ctx.gc_anchor= s7_cons (sc, bv, ctx.gc_anchor);
      s7_gc_protect_via_location (sc, ctx.gc_anchor, ctx.gc_loc);
    }
    if (val.node_id != 0) {
      ctx.reconstructed[val.node_id]= bv;
    }
    uint8_t* p= reinterpret_cast<uint8_t*> (s7_byte_vector_elements (bv));
    if (len > 0) {
      std::memcpy (p, val.bytevec_val->data (), len);
    }
    return bv;
  }
  case GFValueType::Eof:
    return s7_eof_object (sc);
  case GFValueType::Let: {
    // 先建空壳并注册，使环/共享引用可解析，再逐字段填充
    s7_pointer let= s7_inlet (sc, s7_nil (sc));
    if (ctx.gc_loc >= 0) {
      ctx.gc_anchor= s7_cons (sc, let, ctx.gc_anchor);
      s7_gc_protect_via_location (sc, ctx.gc_anchor, ctx.gc_loc);
    }
    if (val.node_id != 0) {
      ctx.reconstructed[val.node_id]= let;
    }
    if (val.pair_val) {
      s7_pointer alist= gfvalue_to_s7_impl (sc, val.pair_val->first, ctx);
      for (s7_pointer p= alist; s7_is_pair (p); p= s7_cdr (p)) {
        s7_pointer entry= s7_car (p);
        if (s7_is_pair (entry) && s7_is_symbol (s7_car (entry))) {
          s7_varlet (sc, let, s7_car (entry), s7_cdr (entry));
        }
      }
    }
    return let;
  }
  case GFValueType::Undefined:
  default:
    return s7_undefined (sc);
  }
}

s7_pointer
gfvalue_to_s7 (s7_scheme* sc, const GFValue& val) {
  DeserializeCtx ctx;
  switch (val.type) {
  // 只有复合类型需要 GC anchor 与环重建表；标量直接转换，零额外开销
  case GFValueType::Pair:
  case GFValueType::Vector:
  case GFValueType::ByteVector:
  case GFValueType::Let:
  case GFValueType::Ref:
    ctx.gc_anchor= s7_nil (sc);
    ctx.gc_loc   = s7_gc_protect (sc, ctx.gc_anchor);
    {
      s7_pointer res= gfvalue_to_s7_impl (sc, val, ctx);
      s7_gc_unprotect_at (sc, ctx.gc_loc);
      return res;
    }
  default:
    return gfvalue_to_s7_impl (sc, val, ctx);
  }
}

// ---------------------------------------------------------------------------
// GoldfishChannel Implementation
// ---------------------------------------------------------------------------

GoldfishChannel::SendStatus
GoldfishChannel::send (GFValue val, int64_t timeout_ms) {
  std::unique_lock<std::mutex> lock (mtx);
  if (closed) return SendStatus::Closed;

  if (capacity == 0) {
    // Unbuffered channel: rendezvous
    if (!waiting_receivers.empty ()) {
      RendezvousReceiver* r= waiting_receivers.front ();
      waiting_receivers.pop_front ();
      *(r->out)   = std::move (val);
      r->completed= true;
      r->cv.notify_one ();
      return SendStatus::Ok;
    }

    if (timeout_ms == 0) {
      return SendStatus::Timeout;
    }

    RendezvousSender s;
    s.val= std::move (val);
    waiting_senders.push_back (&s);

    if (timeout_ms < 0) {
      s.cv.wait (lock, [&s, this] () { return s.completed || closed; });
    }
    else {
      s.cv.wait_for (lock, std::chrono::milliseconds (timeout_ms), [&s, this] () { return s.completed || closed; });
    }

    if (!s.completed) {
      for (auto it= waiting_senders.begin (); it != waiting_senders.end (); ++it) {
        if (*it == &s) {
          waiting_senders.erase (it);
          break;
        }
      }
      return closed ? SendStatus::Closed : SendStatus::Timeout;
    }
    return SendStatus::Ok;
  }
  else {
    // Buffered channel
    if (timeout_ms < 0) {
      cv_buf_send.wait (lock, [this] () { return closed || buffer.size () < capacity; });
    }
    else if (timeout_ms == 0) {
      if (closed) return SendStatus::Closed;
      if (buffer.size () >= capacity) return SendStatus::Timeout;
    }
    else {
      bool ok= cv_buf_send.wait_for (lock, std::chrono::milliseconds (timeout_ms),
                                     [this] () { return closed || buffer.size () < capacity; });
      if (!ok && !closed && buffer.size () >= capacity) {
        return SendStatus::Timeout;
      }
    }

    if (closed) return SendStatus::Closed;

    buffer.push_back (std::move (val));
    cv_buf_recv.notify_one ();
    return SendStatus::Ok;
  }
}

GoldfishChannel::RecvStatus
GoldfishChannel::recv (GFValue& out, int64_t timeout_ms) {
  std::unique_lock<std::mutex> lock (mtx);

  if (capacity == 0) {
    if (!waiting_senders.empty ()) {
      RendezvousSender* s= waiting_senders.front ();
      waiting_senders.pop_front ();
      out         = std::move (s->val);
      s->completed= true;
      s->cv.notify_one ();
      return RecvStatus::Ok;
    }

    if (closed) {
      return RecvStatus::Closed;
    }

    if (timeout_ms == 0) {
      return RecvStatus::Timeout;
    }

    RendezvousReceiver r;
    r.out= &out;
    waiting_receivers.push_back (&r);

    if (timeout_ms < 0) {
      r.cv.wait (lock, [&r, this] () { return r.completed || closed; });
    }
    else {
      r.cv.wait_for (lock, std::chrono::milliseconds (timeout_ms), [&r, this] () { return r.completed || closed; });
    }

    if (!r.completed) {
      for (auto it= waiting_receivers.begin (); it != waiting_receivers.end (); ++it) {
        if (*it == &r) {
          waiting_receivers.erase (it);
          break;
        }
      }
      return closed ? RecvStatus::Closed : RecvStatus::Timeout;
    }
    return RecvStatus::Ok;
  }
  else {
    // Buffered channel
    if (buffer.empty () && !closed) {
      if (timeout_ms == 0) {
        return RecvStatus::Timeout;
      }
      else if (timeout_ms < 0) {
        cv_buf_recv.wait (lock, [this] () { return closed || !buffer.empty (); });
      }
      else {
        cv_buf_recv.wait_for (lock, std::chrono::milliseconds (timeout_ms),
                              [this] () { return closed || !buffer.empty (); });
      }
    }

    if (!buffer.empty ()) {
      out= std::move (buffer.front ());
      buffer.pop_front ();
      cv_buf_send.notify_one ();
      return RecvStatus::Ok;
    }

    return closed ? RecvStatus::Closed : RecvStatus::Timeout;
  }
}

void
GoldfishChannel::close () {
  std::unique_lock<std::mutex> lock (mtx);
  if (closed) return;
  closed= true;

  // Wake up all buffered waiters
  cv_buf_send.notify_all ();
  cv_buf_recv.notify_all ();

  // Wake up all rendezvous waiters
  for (auto* s : waiting_senders) {
    s->cv.notify_one ();
  }
  waiting_senders.clear ();

  for (auto* r : waiting_receivers) {
    r->cv.notify_one ();
  }
  waiting_receivers.clear ();
}

bool
GoldfishChannel::is_closed () const {
  std::lock_guard<std::mutex> lock (mtx);
  return closed;
}

// ---------------------------------------------------------------------------
// S7 Glue Functions
// ---------------------------------------------------------------------------

static s7_pointer
f_make_chan (s7_scheme* sc, s7_pointer args) {
  s7_int cap= 0;
  if (!s7_is_null (sc, args)) {
    s7_pointer cap_arg= s7_car (args);
    if (!s7_is_integer (cap_arg) || s7_integer (cap_arg) < 0) {
      return go_error (sc, "type-error", "make-chan: capacity must be a non-negative integer", cap_arg);
    }
    cap= s7_integer (cap_arg);
  }
  auto ch= std::make_shared<GoldfishChannel> (static_cast<size_t> (cap));
  return make_goldfish_channel_object (sc, ch);
}

static s7_pointer
f_chan_p (s7_scheme* sc, s7_pointer args) {
  return s7_make_boolean (sc, is_goldfish_channel (sc, s7_car (args)));
}

static s7_pointer
f_chan_send (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch_arg = s7_car (args);
  s7_pointer val_arg= s7_cadr (args);

  // 以下检查点只允许平凡局部变量存活（见 go_error 的 longjmp 约束注释）
  if (!is_goldfish_channel (sc, ch_arg)) {
    return go_error (sc, "type-error", "chan-send!: first argument must be a channel", ch_arg);
  }

  int64_t    timeout_ms= -1; // Default -1: infinite wait (Go channel semantics)
  s7_pointer rest      = s7_cddr (args);
  if (!s7_is_null (sc, rest)) {
    s7_pointer to_arg= s7_car (rest);
    if (!s7_is_integer (to_arg) || s7_integer (to_arg) < 0) {
      return go_error (sc, "type-error", "chan-send!: timeout must be a non-negative integer (milliseconds)", to_arg);
    }
    timeout_ms= s7_integer (to_arg);
  }

  // RAII 对象（GFValue/shared_ptr/string）限制在内层作用域，出作用域后再 raise
  GoldfishChannel::SendStatus status= GoldfishChannel::SendStatus::Timeout;
  bool                        ser_ok= false;
  {
    GFValue val;
    ser_ok= s7_to_gfvalue (sc, val_arg, val, t_serialize_err);
    if (ser_ok) {
      status= get_goldfish_channel (sc, ch_arg)->send (std::move (val), timeout_ms);
    }
  }

  if (!ser_ok) {
    return go_error (sc, "type-error", t_serialize_err.c_str (), val_arg);
  }
  if (status == GoldfishChannel::SendStatus::Closed) {
    return go_error (sc, "value-error", "chan-send!: cannot send on closed channel", ch_arg);
  }
  else if (status == GoldfishChannel::SendStatus::Timeout) {
    return s7_f (sc);
  }
  return s7_t (sc);
}

static s7_pointer
f_chan_recv (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch_arg= s7_car (args);

  // raise 时只允许平凡局部变量存活（见 go_error 的 longjmp 约束注释）
  if (!is_goldfish_channel (sc, ch_arg)) {
    return go_error (sc, "type-error", "chan-recv!: first argument must be a channel", ch_arg);
  }

  int64_t    timeout_ms = -1; // Default -1: infinite wait (Go channel semantics)
  s7_pointer default_val= s7_make_symbol (sc, "timeout");

  s7_pointer rest= s7_cdr (args);
  if (!s7_is_null (sc, rest)) {
    s7_pointer to_arg= s7_car (rest);
    if (!s7_is_integer (to_arg) || s7_integer (to_arg) < 0) {
      return go_error (sc, "type-error", "chan-recv!: timeout must be a non-negative integer (milliseconds)", to_arg);
    }
    timeout_ms= s7_integer (to_arg);

    s7_pointer rest2= s7_cdr (rest);
    if (!s7_is_null (sc, rest2)) {
      default_val= s7_car (rest2);
    }
  }

  auto    ch= get_goldfish_channel (sc, ch_arg);
  GFValue val;
  auto    status= ch->recv (val, timeout_ms);

  if (status == GoldfishChannel::RecvStatus::Closed) {
    return s7_eof_object (sc);
  }
  else if (status == GoldfishChannel::RecvStatus::Timeout) {
    return default_val;
  }
  return gfvalue_to_s7 (sc, val);
}

static s7_pointer
f_chan_try_recv (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch_arg= s7_car (args);

  if (!is_goldfish_channel (sc, ch_arg)) {
    return go_error (sc, "type-error", "chan-try-recv!: first argument must be a channel", ch_arg);
  }

  s7_pointer default_val= s7_f (sc);
  s7_pointer rest       = s7_cdr (args);
  if (!s7_is_null (sc, rest)) {
    default_val= s7_car (rest);
  }

  auto    ch= get_goldfish_channel (sc, ch_arg);
  GFValue val;
  auto    status= ch->try_recv (val);

  if (status == GoldfishChannel::RecvStatus::Ok) {
    return gfvalue_to_s7 (sc, val);
  }
  else if (status == GoldfishChannel::RecvStatus::Closed) {
    return s7_eof_object (sc);
  }
  else {
    // Timeout (非阻塞语义下即为通道为空)
    return default_val;
  }
}

static s7_pointer
f_chan_close (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch_arg= s7_car (args);

  if (!is_goldfish_channel (sc, ch_arg)) {
    return go_error (sc, "type-error", "chan-close!: argument must be a channel", ch_arg);
  }

  auto ch= get_goldfish_channel (sc, ch_arg);
  ch->close ();
  return s7_unspecified (sc);
}

static s7_pointer
f_chan_closed_p (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch_arg= s7_car (args);

  if (!is_goldfish_channel (sc, ch_arg)) {
    return go_error (sc, "type-error", "chan-closed?: argument must be a channel", ch_arg);
  }

  auto ch= get_goldfish_channel (sc, ch_arg);
  return s7_make_boolean (sc, ch->is_closed ());
}

struct SpawnError {
  const char* kind= nullptr;
  const char* msg = nullptr;
  s7_pointer  arg = nullptr;
};

// 所有 RAII 对象（GoTask/GFValue/string）都在本函数帧内析构；
// 调用方拿到错误信息后再 raise（见 go_error 的 longjmp 约束注释）。
static SpawnError
try_spawn_task (s7_scheme* sc, s7_pointer names_arg, s7_pointer vals_arg, s7_pointer code_arg) {
  GoTask     task;
  s7_pointer cur_name= names_arg;
  while (s7_is_pair (cur_name)) {
    s7_pointer sym= s7_car (cur_name);
    if (!s7_is_symbol (sym)) {
      return {"type-error", "go: variable name must be a symbol", sym};
    }
    task.var_names.push_back (s7_symbol_name (sym));
    cur_name= s7_cdr (cur_name);
  }

  s7_pointer cur_val= vals_arg;
  while (s7_is_pair (cur_val)) {
    GFValue val;
    if (!s7_to_gfvalue (sc, s7_car (cur_val), val, t_serialize_err)) {
      // t_serialize_err 是 TLS，在调用方 raise 时仍然有效
      return {"type-error", t_serialize_err.c_str (), s7_car (cur_val)};
    }
    task.var_vals.push_back (std::move (val));
    cur_val= s7_cdr (cur_val);
  }

  if (task.var_names.size () != task.var_vals.size ()) {
    return {"value-error", "go: variable names and values count mismatch", names_arg};
  }

  if (!s7_to_gfvalue (sc, code_arg, task.code_expr, t_serialize_err)) {
    return {"type-error", t_serialize_err.c_str (), code_arg};
  }

  GoThreadPool::instance ().enqueue (std::move (task));
  return {};
}

static s7_pointer
f_go_spawn (s7_scheme* sc, s7_pointer args) {
  SpawnError err= try_spawn_task (sc, s7_car (args), s7_cadr (args), s7_caddr (args));
  if (err.kind != nullptr) {
    return go_error (sc, err.kind, err.msg, err.arg);
  }
  return s7_unspecified (sc);
}

static s7_pointer
f_go_worker_count (s7_scheme* sc, s7_pointer args) {
  // 只返回配置值，不触发线程池构造
  return s7_make_integer (sc, static_cast<s7_int> (configured_worker_count ()));
}

static s7_pointer
f_chan_timeout_close (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch_arg= s7_car (args);
  s7_pointer ms_arg= s7_cadr (args);

  // raise 时只允许平凡局部变量存活（见 go_error 的 longjmp 约束注释）
  if (!is_goldfish_channel (sc, ch_arg)) {
    return go_error (sc, "type-error", "g_chan-timeout-close!: first argument must be a channel", ch_arg);
  }
  if (!s7_is_integer (ms_arg) || s7_integer (ms_arg) < 0) {
    return go_error (sc, "type-error", "g_chan-timeout-close!: ms must be a non-negative integer", ms_arg);
  }

  GoTimer::instance ().schedule_close (get_goldfish_channel (sc, ch_arg), s7_integer (ms_arg));
  return s7_unspecified (sc);
}

static s7_pointer
f_msleep (s7_scheme* sc, s7_pointer args) {
  s7_pointer ms_arg= s7_car (args);
  if (!s7_is_integer (ms_arg) || s7_integer (ms_arg) < 0) {
    return go_error (sc, "type-error", "g_msleep: ms must be a non-negative integer", ms_arg);
  }
  int64_t ms= s7_integer (ms_arg);
  if (ms > 0) {
    std::this_thread::sleep_for (std::chrono::milliseconds (ms));
  }
  else {
    std::this_thread::yield ();
  }
  return s7_unspecified (sc);
}

static s7_pointer
f_now_ms (s7_scheme* sc, s7_pointer args) {
  auto now= std::chrono::steady_clock::now ();
  auto ms = std::chrono::duration_cast<std::chrono::milliseconds> (now.time_since_epoch ()).count ();
  return s7_make_integer (sc, static_cast<s7_int> (ms));
}

void
glue_liii_go (s7_scheme* sc) {
  // Register C-Type for channel
  s7_int tag= s7_make_c_type (sc, "channel");
  s7_c_type_set_free (sc, tag, channel_free_c_value);
  s7_c_type_set_to_string (sc, tag, channel_to_string_glue);
  s7_c_type_set_is_equal (sc, tag, channel_is_equal_glue);

  {
    std::lock_guard<std::mutex> lock (g_type_mtx);
    g_channel_type_tags[sc]= tag;
  }

  s7_define_function (sc, "g_make-chan", f_make_chan, 0, 1, false, "(g_make-chan [capacity]) => channel");
  s7_define_function (sc, "g_chan?", f_chan_p, 1, 0, false, "(g_chan? obj) => boolean");
  s7_define_function (sc, "g_chan-send!", f_chan_send, 2, 1, false, "(g_chan-send! ch val [timeout-ms]) => boolean");
  s7_define_function (sc, "g_chan-recv!", f_chan_recv, 1, 2, false,
                      "(g_chan-recv! ch [timeout-ms [default]]) => value | default | eof-object");
  s7_define_function (sc, "g_chan-try-recv!", f_chan_try_recv, 1, 1, false,
                      "(g_chan-try-recv! ch [default]) => value | default | eof-object");
  s7_define_function (sc, "g_chan-close!", f_chan_close, 1, 0, false, "(g_chan-close! ch) => unspecified");
  s7_define_function (sc, "g_chan-closed?", f_chan_closed_p, 1, 0, false, "(g_chan-closed? ch) => boolean");
  s7_define_function (sc, "g_go-spawn", f_go_spawn, 3, 0, false, "(g_go-spawn names vals code) => unspecified");
  s7_define_function (sc, "g_worker-notify-error", f_worker_notify_error, 2, 0, false,
                      "(g_worker-notify-error tag args) => unspecified");
  s7_define_function (sc, "g_go-worker-count", f_go_worker_count, 0, 0, false, "(g_go-worker-count) => integer");
  s7_define_function (sc, "g_chan-timeout-close!", f_chan_timeout_close, 2, 0, false,
                      "(g_chan-timeout-close! ch ms) => unspecified, closes ch after ms milliseconds");
  s7_define_function (sc, "g_msleep", f_msleep, 1, 0, false, "(g_msleep ms) => unspecified");
  s7_define_function (sc, "g_now-ms", f_now_ms, 0, 0, false, "(g_now-ms) => integer");
}

} // namespace goldfish
