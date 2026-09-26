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
#include <cstring>
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

static thread_local bool t_worker_task_failed= false;

static s7_pointer
f_worker_notify_error (s7_scheme* sc, s7_pointer args) {
  t_worker_task_failed= true;
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

  size_t worker_count () const { return workers.size (); }

private:
  GoThreadPool () {
    size_t n= std::thread::hardware_concurrency ();
    if (n == 0) n= 4;
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

      s7_pointer body      = gfvalue_to_s7 (worker_sc, task.code_expr);
      s7_pointer lambda_sym= s7_make_symbol (worker_sc, "lambda");
      s7_pointer catch_sym = s7_make_symbol (worker_sc, "catch");
      s7_pointer body_thunk= s7_list (worker_sc, 3, lambda_sym, s7_nil (worker_sc), body);

      // (lambda (err-tag err-args) (g_worker-notify-error err-tag err-args))
      s7_pointer notify_call= s7_list (worker_sc, 3, s7_make_symbol (worker_sc, "g_worker-notify-error"),
                                       s7_make_symbol (worker_sc, "err-tag"), s7_make_symbol (worker_sc, "err-args"));

      s7_pointer err_handler= s7_list (
          worker_sc, 3, lambda_sym,
          s7_list (worker_sc, 2, s7_make_symbol (worker_sc, "err-tag"), s7_make_symbol (worker_sc, "err-args")),
          notify_call);

      s7_pointer catch_expr= s7_list (worker_sc, 4, catch_sym, s7_t (worker_sc), body_thunk, err_handler);

      s7_pointer let_sym = s7_make_symbol (worker_sc, "let");
      s7_pointer let_expr= s7_cons (worker_sc, let_sym,
                                    s7_cons (worker_sc, bindings, s7_cons (worker_sc, catch_expr, s7_nil (worker_sc))));

      t_worker_task_failed= false;
      s7_eval (worker_sc, let_expr, s7_rootlet (worker_sc));

      if (t_worker_task_failed) {
        // Automatically close all channels passed to the failed task to unblock receivers
        for (const auto& v : task.var_vals) {
          if (v.type == GFValueType::Channel && v.chan_val) {
            v.chan_val->close ();
          }
        }
      }
    }
  }

  std::vector<std::thread> workers;
  std::queue<GoTask>       tasks;
  std::mutex               mtx;
  std::condition_variable  cv;
  bool                     stop= false;
};

static std::mutex                             g_type_mtx;
static std::unordered_map<s7_scheme*, s7_int> g_channel_type_tags;

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

bool
s7_to_gfvalue (s7_scheme* sc, s7_pointer obj, GFValue& out, std::string& err_msg, int depth) {
  if (depth > 2048) {
    err_msg= "maximum recursion depth exceeded during serialization";
    return false;
  }

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
    const char* str= s7_object_to_c_string (sc, obj);
    if (str != nullptr && std::strncmp (str, "#_", 2) == 0) {
      out.type   = GFValueType::Symbol;
      out.str_val= str + 2;
      return true;
    }
  }
  if (is_goldfish_channel (sc, obj)) {
    out.type    = GFValueType::Channel;
    out.chan_val= get_goldfish_channel (sc, obj);
    return true;
  }
  if (s7_is_pair (obj)) {
    out.type     = GFValueType::Pair;
    auto pair_ptr= std::make_shared<std::pair<GFValue, GFValue>> ();
    if (!s7_to_gfvalue (sc, s7_car (obj), pair_ptr->first, err_msg, depth + 1)) return false;
    if (!s7_to_gfvalue (sc, s7_cdr (obj), pair_ptr->second, err_msg, depth + 1)) return false;
    out.pair_val= pair_ptr;
    return true;
  }
  if (s7_is_byte_vector (obj)) {
    out.type            = GFValueType::ByteVector;
    s7_int         len  = s7_vector_length (obj);
    const uint8_t* bytes= reinterpret_cast<const uint8_t*> (s7_byte_vector_elements (obj));
    out.bytevec_val     = std::make_shared<std::vector<uint8_t>> (bytes, bytes + len);
    return true;
  }
  if (s7_is_vector (obj)) {
    out.type      = GFValueType::Vector;
    s7_int len    = s7_vector_length (obj);
    auto   vec_ptr= std::make_shared<std::vector<GFValue>> ();
    vec_ptr->resize (len);
    for (s7_int i= 0; i < len; ++i) {
      s7_pointer elem= s7_vector_ref (sc, obj, i);
      if (!s7_to_gfvalue (sc, elem, (*vec_ptr)[i], err_msg, depth + 1)) return false;
    }
    out.vec_val= vec_ptr;
    return true;
  }
  if (obj == s7_eof_object (sc)) {
    out.type= GFValueType::Eof;
    return true;
  }

  err_msg= std::string ("unsupported object type for channel serialization: ") + s7_object_to_c_string (sc, obj);
  return false;
}

s7_pointer
gfvalue_to_s7 (s7_scheme* sc, const GFValue& val) {
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
    if (!val.pair_val) return s7_nil (sc);
    s7_pointer car_p= gfvalue_to_s7 (sc, val.pair_val->first);
    s7_pointer cdr_p= gfvalue_to_s7 (sc, val.pair_val->second);
    return s7_cons (sc, car_p, cdr_p);
  }
  case GFValueType::ByteVector: {
    if (!val.bytevec_val) return s7_make_byte_vector (sc, 0, 1, nullptr);
    s7_int     len= static_cast<s7_int> (val.bytevec_val->size ());
    s7_pointer bv = s7_make_byte_vector (sc, len, 1, nullptr);
    uint8_t*   p  = reinterpret_cast<uint8_t*> (s7_byte_vector_elements (bv));
    if (len > 0) {
      std::memcpy (p, val.bytevec_val->data (), len);
    }
    return bv;
  }
  case GFValueType::Vector: {
    if (!val.vec_val) return s7_make_vector (sc, 0);
    s7_int     len= static_cast<s7_int> (val.vec_val->size ());
    s7_pointer vec= s7_make_vector (sc, len);
    for (s7_int i= 0; i < len; ++i) {
      s7_vector_set (sc, vec, i, gfvalue_to_s7 (sc, (*val.vec_val)[i]));
    }
    return vec;
  }
  case GFValueType::Eof:
    return s7_eof_object (sc);
  case GFValueType::Undefined:
  default:
    return s7_undefined (sc);
  }
}

// ---------------------------------------------------------------------------
// GoldfishChannel Implementation
// ---------------------------------------------------------------------------

GoldfishChannel::SendStatus
GoldfishChannel::send (const GFValue& val, int64_t timeout_ms) {
  std::unique_lock<std::mutex> lock (mtx);
  if (closed) return SendStatus::Closed;

  if (capacity == 0) {
    // Unbuffered channel: rendezvous
    if (!waiting_receivers.empty ()) {
      RendezvousReceiver* r= waiting_receivers.front ();
      waiting_receivers.pop_front ();
      if (r->out != nullptr) {
        *(r->out)= val;
      }
      r->completed= true;
      r->cv.notify_one ();
      return SendStatus::Ok;
    }

    if (timeout_ms == 0) {
      return SendStatus::Timeout;
    }

    RendezvousSender s;
    s.val= val;
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

    buffer.push_back (val);
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
      out         = s->val;
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
      out= buffer.front ();
      buffer.pop_front ();
      cv_buf_send.notify_one ();
      return RecvStatus::Ok;
    }

    return closed ? RecvStatus::Closed : RecvStatus::Timeout;
  }
}

GoldfishChannel::RecvStatus
GoldfishChannel::try_recv (GFValue& out) {
  auto status= recv (out, 0);
  if (status == RecvStatus::Timeout) {
    return RecvStatus::Empty;
  }
  return status;
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

size_t
GoldfishChannel::size () const {
  std::lock_guard<std::mutex> lock (mtx);
  return buffer.size ();
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

  if (!is_goldfish_channel (sc, ch_arg)) {
    return go_error (sc, "type-error", "chan-send!: first argument must be a channel", ch_arg);
  }

  auto ch= get_goldfish_channel (sc, ch_arg);
  if (ch->is_closed ()) {
    return go_error (sc, "value-error", "chan-send!: cannot send on closed channel", ch_arg);
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

  GFValue     val;
  std::string err_msg;
  if (!s7_to_gfvalue (sc, val_arg, val, err_msg)) {
    return go_error (sc, "type-error", err_msg.c_str (), val_arg);
  }

  auto status= ch->send (val, timeout_ms);
  if (status == GoldfishChannel::SendStatus::Closed) {
    return go_error (sc, "value-error", "chan-send!: channel closed during send", ch_arg);
  }
  else if (status == GoldfishChannel::SendStatus::Timeout) {
    return s7_f (sc);
  }
  return s7_t (sc);
}

static s7_pointer
f_chan_recv (s7_scheme* sc, s7_pointer args) {
  s7_pointer ch_arg= s7_car (args);

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
    // Empty
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

static s7_pointer
f_go_spawn (s7_scheme* sc, s7_pointer args) {
  s7_pointer names_arg= s7_car (args);
  s7_pointer vals_arg = s7_cadr (args);
  s7_pointer code_arg = s7_caddr (args);

  GoTask     task;
  s7_pointer cur_name= names_arg;
  while (s7_is_pair (cur_name)) {
    s7_pointer sym= s7_car (cur_name);
    if (!s7_is_symbol (sym)) {
      return go_error (sc, "type-error", "go: variable name must be a symbol", sym);
    }
    task.var_names.push_back (s7_symbol_name (sym));
    cur_name= s7_cdr (cur_name);
  }

  s7_pointer cur_val= vals_arg;
  while (s7_is_pair (cur_val)) {
    GFValue     val;
    std::string err;
    if (!s7_to_gfvalue (sc, s7_car (cur_val), val, err)) {
      return go_error (sc, "type-error", err.c_str (), s7_car (cur_val));
    }
    task.var_vals.push_back (std::move (val));
    cur_val= s7_cdr (cur_val);
  }

  std::string err;
  if (!s7_to_gfvalue (sc, code_arg, task.code_expr, err)) {
    return go_error (sc, "type-error", err.c_str (), code_arg);
  }

  GoThreadPool::instance ().enqueue (std::move (task));
  return s7_unspecified (sc);
}

static s7_pointer
f_go_worker_count (s7_scheme* sc, s7_pointer args) {
  return s7_make_integer (sc, static_cast<s7_int> (GoThreadPool::instance ().worker_count ()));
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
  s7_define_function (sc, "g_msleep", f_msleep, 1, 0, false, "(g_msleep ms) => unspecified");
  s7_define_function (sc, "g_now-ms", f_now_ms, 0, 0, false, "(g_now-ms) => integer");
}

} // namespace goldfish
