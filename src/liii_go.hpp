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

#ifndef LIII_GO_HPP
#define LIII_GO_HPP

#include "s7.h"
#include <condition_variable>
#include <deque>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

namespace goldfish {

class GoldfishChannel;

enum class GFValueType {
  Nil,
  Boolean,
  Integer,
  Real,
  Character,
  String,
  Symbol,
  Pair,
  Vector,
  ByteVector,
  Channel,
  Eof,
  Ref,
  Undefined
};

struct GFValue {
  GFValueType                                  type      = GFValueType::Undefined;
  size_t                                       node_id   = 0;
  size_t                                       ref_id    = 0;
  bool                                         bool_val  = false;
  s7_int                                       int_val   = 0;
  s7_double                                    double_val= 0.0;
  uint32_t                                     char_val  = 0;
  std::string                                  str_val;
  std::shared_ptr<std::pair<GFValue, GFValue>> pair_val;
  std::shared_ptr<std::vector<GFValue>>        vec_val;
  std::shared_ptr<const std::vector<uint8_t>>  bytevec_val;
  std::shared_ptr<GoldfishChannel>             chan_val;
};

// S7 pointer <-> GFValue conversion
bool       s7_to_gfvalue (s7_scheme* sc, s7_pointer obj, GFValue& out, std::string& err_msg);
s7_pointer gfvalue_to_s7 (s7_scheme* sc, const GFValue& val);

class GoldfishChannel {
public:
  explicit GoldfishChannel (size_t cap= 0) : capacity (cap), closed (false) {}
  ~GoldfishChannel () { close (); }

  enum class SendStatus { Ok, Closed, Timeout };

  enum class RecvStatus {
    Ok,
    Closed,
    Timeout,
    Empty // for non-blocking try_recv
  };

  // timeout_ms < 0 means infinite wait
  SendStatus send (const GFValue& val, int64_t timeout_ms= -1);
  RecvStatus recv (GFValue& out, int64_t timeout_ms= -1);
  RecvStatus try_recv (GFValue& out);

  void   close ();
  bool   is_closed () const;
  size_t get_capacity () const { return capacity; }
  size_t size () const;

private:
  struct RendezvousSender {
    GFValue                 val;
    bool                    completed= false;
    std::condition_variable cv;
  };

  struct RendezvousReceiver {
    GFValue*                out      = nullptr;
    bool                    completed= false;
    std::condition_variable cv;
  };

  size_t             capacity; // 0: unbuffered (rendezvous), > 0: buffered
  bool               closed;
  mutable std::mutex mtx;

  // Buffered channel synchronization
  std::condition_variable cv_buf_send;
  std::condition_variable cv_buf_recv;
  std::deque<GFValue>     buffer;

  // Unbuffered channel synchronization (rendezvous queues)
  std::deque<RendezvousSender*>   waiting_senders;
  std::deque<RendezvousReceiver*> waiting_receivers;
};

void        set_goldfish_lib_dir (const std::string& dir);
std::string get_goldfish_lib_dir ();

bool                             is_goldfish_channel (s7_scheme* sc, s7_pointer obj);
std::shared_ptr<GoldfishChannel> get_goldfish_channel (s7_scheme* sc, s7_pointer obj);
s7_pointer                       make_goldfish_channel_object (s7_scheme* sc, std::shared_ptr<GoldfishChannel> ch);

void glue_liii_go (s7_scheme* sc);

} // namespace goldfish

#endif // LIII_GO_HPP
