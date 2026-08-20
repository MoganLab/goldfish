//
// Copyright (C) 2024 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

#include "gf.h"
#include <algorithm>
#include <chrono>
#include <cpr/cpr.h>
#include <cstring>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

namespace goldfish {
using std::string;
using std::vector;

static gf::pointer
error2hashtable (gf::scheme* sc, long status_code, const std::string& url, const std::string& reason) {
  gf::pointer ht= gf::make_hash_table (sc, 4);
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "status-code"), gf::make_integer (sc, status_code));
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "url"), gf::make_string (sc, url.c_str ()));
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "text"), gf::make_string (sc, ""));
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "reason"), gf::make_string (sc, reason.c_str ()));
  return ht;
}

static gf::pointer
response2hashtable (gf::scheme* sc, cpr::Response r) {
  gf::pointer ht= gf::make_hash_table (sc, 8);
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "status-code"), gf::make_integer (sc, r.status_code));
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "url"), gf::make_string (sc, r.url.c_str ()));
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "elapsed"), gf::make_real (sc, r.elapsed));
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "text"), gf::make_string (sc, r.text.c_str ()));
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "reason"), gf::make_string (sc, r.reason.c_str ()));
  gf::pointer headers= gf::make_hash_table (sc, r.header.size ());
  for (const auto& header : r.header) {
    const auto  key      = header.first.c_str ();
    std::string key_lower= header.first;
    std::transform (key_lower.begin (), key_lower.end (), key_lower.begin (), ::tolower);
    const auto value= header.second.c_str ();
    gf::hash_table_set (sc, headers, gf::make_string (sc, key_lower.c_str ()), gf::make_string (sc, value));
  }
  gf::hash_table_set (sc, ht, gf::make_symbol (sc, "headers"), headers);

  return ht;
}

inline cpr::Parameters
to_cpr_parameters (gf::scheme* sc, gf::pointer args) {
  cpr::Parameters params= cpr::Parameters{};
  gf::pointer      iter  = args;
  while (!gf::is_null (sc, iter)) {
    gf::pointer pair= gf::car (iter);
    params.Add (cpr::Parameter (gf::string (gf::car (pair)), gf::string (gf::cdr (pair))));
    iter= gf::cdr (iter);
  }
  return params;
}

inline cpr::Header
to_cpr_headers (gf::scheme* sc, gf::pointer args) {
  cpr::Header headers= cpr::Header{};
  gf::pointer  iter   = args;
  while (!gf::is_null (sc, iter)) {
    gf::pointer pair= gf::car (iter);
    headers.insert ({gf::string (gf::car (pair)), gf::string (gf::cdr (pair))});
    iter= gf::cdr (iter);
  }
  return headers;
}

inline cpr::Proxies
to_cpr_proxies (gf::scheme* sc, gf::pointer args) {
  std::map<std::string, std::string> proxy_map;
  gf::pointer                         iter= args;
  while (!gf::is_null (sc, iter)) {
    gf::pointer pair                     = gf::car (iter);
    proxy_map[gf::string (gf::car (pair))]= gf::string (gf::cdr (pair));
    iter                                = gf::cdr (iter);
  }
  return cpr::Proxies (proxy_map);
}

static cpr::Part
to_cpr_multipart_part (gf::scheme* sc, gf::pointer part_spec) {
  std::string name;
  std::string value;
  std::string file_path;
  std::string filename;
  std::string content_type;
  bool        has_file= false;

  gf::pointer iter= part_spec;
  while (!gf::is_null (sc, iter)) {
    gf::pointer  entry    = gf::car (iter);
    gf::pointer  raw_key  = gf::car (entry);
    const char* key      = gf::is_symbol (raw_key) ? gf::symbol_name (raw_key) : gf::string (raw_key);
    const char* raw_value= gf::string (gf::cdr (entry));

    if (strcmp (key, "name") == 0) {
      name= raw_value;
    }
    else if (strcmp (key, "value") == 0) {
      value= raw_value;
    }
    else if (strcmp (key, "file") == 0) {
      file_path= raw_value;
      has_file = true;
    }
    else if (strcmp (key, "filename") == 0) {
      filename= raw_value;
    }
    else if (strcmp (key, "content-type") == 0) {
      content_type= raw_value;
    }

    iter= gf::cdr (iter);
  }

  if (has_file) {
    cpr::Files files;
    if (filename.empty ()) {
      files.push_back (cpr::File (file_path));
    }
    else {
      files.push_back (cpr::File (file_path, filename));
    }
    return cpr::Part (name, files, content_type);
  }

  return cpr::Part (name, value, content_type);
}

static void
append_cpr_multipart_file_parts (gf::scheme* sc, gf::pointer files, std::vector<cpr::Part>& parts) {
  gf::pointer iter= files;
  while (!gf::is_null (sc, iter)) {
    parts.push_back (to_cpr_multipart_part (sc, gf::car (iter)));
    iter= gf::cdr (iter);
  }
}

static void
append_cpr_multipart_form_parts (gf::scheme* sc, gf::pointer data, std::vector<cpr::Part>& parts) {
  gf::pointer iter= data;
  while (!gf::is_null (sc, iter)) {
    gf::pointer pair= gf::car (iter);
    parts.push_back (cpr::Part (gf::string (gf::car (pair)), gf::string (gf::cdr (pair))));
    iter= gf::cdr (iter);
  }
}

static cpr::Multipart
to_cpr_post_multipart (gf::scheme* sc, gf::pointer data, gf::pointer files) {
  std::vector<cpr::Part> parts;
  append_cpr_multipart_form_parts (sc, data, parts);
  append_cpr_multipart_file_parts (sc, files, parts);
  return cpr::Multipart (parts);
}

static gf::pointer
f_http_head (gf::scheme* sc, gf::pointer args) {
  const char*  url= gf::string (gf::car (args));
  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  cpr::Response r= session.Head ();
  return response2hashtable (sc, r);
}

inline void
glue_http_head (gf::scheme* sc) {
  gf::pointer  cur_env       = gf::curlet (sc);
  const char* s_http_head   = "g_http-head";
  const char* d_http_head   = "(g_http-head url ...) => hash-table?";
  auto        func_http_head= gf::make_typed_function (sc, s_http_head, f_http_head, 1, 0, false, d_http_head, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, s_http_head), func_http_head);
}

static gf::pointer
f_http_get (gf::scheme* sc, gf::pointer args) {
  const char*     url        = gf::string (gf::car (args));
  gf::pointer      params     = gf::cadr (args);
  gf::pointer      headers    = gf::caddr (args);
  gf::pointer      proxy      = gf::cadddr (args);
  gf::pointer      callback   = gf::car (gf::cddddr (args));
  cpr::Parameters cpr_params = to_cpr_parameters (sc, params);
  cpr::Header     cpr_headers= to_cpr_headers (sc, headers);
  cpr::Proxies    cpr_proxies= to_cpr_proxies (sc, proxy);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);
  session.SetHeader (cpr_headers);
  if (gf::is_list (sc, proxy) && !gf::is_null (sc, proxy)) {
    session.SetProxies (cpr_proxies);
  }

  if (gf::is_procedure (callback)) {
    session.SetWriteCallback (cpr::WriteCallback{[sc, callback] (const std::string_view& data, intptr_t) -> bool {
      gf::pointer data_str = gf::make_string_with_length (sc, data.data (), data.length ());
      gf::pointer call_args= gf::cons (sc, data_str, gf::nil (sc));

      gf::pointer ret= gf::call (sc, callback, call_args);
      if (gf::is_boolean (ret)) {
        return gf::boolean (sc, ret);
      }

      return true;
    }});

    try {
      cpr::Response response= session.Get ();
      return response2hashtable (sc, response);
    } catch (const std::exception& e) {
      return error2hashtable (sc, 0, url, e.what ());
    }
  }

  cpr::Response r= session.Get ();
  return response2hashtable (sc, r);
}

inline void
glue_http_get (gf::scheme* sc) {
  gf::pointer  cur_env      = gf::curlet (sc);
  const char* s_http_get   = "g_http-get";
  const char* d_http_get   = "(g_http-get url params headers proxy callback) => hash-table? | undefined";
  auto        func_http_get= gf::make_typed_function (sc, s_http_get, f_http_get, 5, 0, false, d_http_get, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, s_http_get), func_http_get);
}

static gf::pointer
f_http_post (gf::scheme* sc, gf::pointer args) {
  const char*     url         = gf::string (gf::car (args));
  gf::pointer      params      = gf::cadr (args);
  gf::pointer      body_or_data= gf::caddr (args);
  gf::pointer      headers     = gf::cadddr (args);
  gf::pointer      proxy       = gf::car (gf::cddddr (args));
  gf::pointer      files       = gf::cadr (gf::cddddr (args));
  gf::pointer      callback    = gf::list_ref (sc, args, 6);
  cpr::Parameters cpr_params  = to_cpr_parameters (sc, params);
  cpr::Header     cpr_headers = to_cpr_headers (sc, headers);
  cpr::Proxies    cpr_proxies = to_cpr_proxies (sc, proxy);

  cpr::Session session;
  session.SetUrl (cpr::Url (url));
  session.SetParameters (cpr_params);
  session.SetHeader (cpr_headers);
  if (gf::is_list (sc, proxy) && !gf::is_null (sc, proxy)) {
    session.SetProxies (cpr_proxies);
  }

  if (gf::is_list (sc, files) && !gf::is_null (sc, files)) {
    session.SetMultipart (to_cpr_post_multipart (sc, body_or_data, files));
  }
  else {
    const char* body    = gf::string (body_or_data);
    cpr::Body   cpr_body= cpr::Body (body);
    session.SetBody (cpr_body);
  }

  if (gf::is_procedure (callback)) {
    session.SetWriteCallback (cpr::WriteCallback{[sc, callback] (const std::string_view& data, intptr_t) -> bool {
      gf::pointer data_str = gf::make_string_with_length (sc, data.data (), data.length ());
      gf::pointer call_args= gf::cons (sc, data_str, gf::nil (sc));

      gf::pointer ret= gf::call (sc, callback, call_args);
      if (gf::is_boolean (ret)) {
        return gf::boolean (sc, ret);
      }

      return true;
    }});

    try {
      cpr::Response response= session.Post ();
      return response2hashtable (sc, response);
    } catch (const std::exception& e) {
      return error2hashtable (sc, 0, url, e.what ());
    }
  }

  cpr::Response r= session.Post ();
  return response2hashtable (sc, r);
}

inline void
glue_http_post (gf::scheme* sc) {
  gf::pointer  cur_env= gf::curlet (sc);
  const char* name   = "g_http-post";
  const char* doc    = "(g_http-post url params body-or-data headers proxy files callback) => hash-table? | undefined";
  auto        func_http_post= gf::make_typed_function (sc, name, f_http_post, 7, 0, false, doc, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, name), func_http_post);
}

void
glue_http (gf::scheme* sc) {
  glue_http_head (sc);
  glue_http_get (sc);
  glue_http_post (sc);
}

// -------------------------------- Async HTTP --------------------------------
// Data structure to store async HTTP request state
struct AsyncHttpRequest {
  gf::scheme*                    sc;
  gf::pointer                    callback;
  int                           gc_loc;
  std::shared_ptr<cpr::Session> session; // Keep session alive
  cpr::AsyncResponse            async_response;
  bool                          completed;
  cpr::Response                 response;
  std::mutex                    mutex;

  AsyncHttpRequest (gf::scheme* scheme, gf::pointer cb, int gc_protect_loc, std::shared_ptr<cpr::Session> sess,
                    cpr::AsyncResponse&& ar)
      : sc (scheme), callback (cb), gc_loc (gc_protect_loc), session (std::move (sess)),
        async_response (std::move (ar)), completed (false) {}
};

// Global list of pending async requests
static std::mutex                                     g_async_requests_mutex;
static std::vector<std::shared_ptr<AsyncHttpRequest>> g_async_requests;

// Check if any async requests have completed and process their callbacks
// This function should be called periodically from the main thread
// Returns the number of callbacks executed
static int
process_async_http_callbacks () {
  std::vector<std::shared_ptr<AsyncHttpRequest>> completed_requests;

  // Find completed requests
  {
    std::lock_guard<std::mutex> lock (g_async_requests_mutex);
    for (auto it= g_async_requests.begin (); it != g_async_requests.end ();) {
      bool is_ready= false;
      {
        std::lock_guard<std::mutex> req_lock ((*it)->mutex);
        if (!(*it)->completed) {
          // Check if the future is ready (non-blocking)
          if ((*it)->async_response.wait_for (std::chrono::seconds (0)) == std::future_status::ready) {
            (*it)->response = (*it)->async_response.get ();
            (*it)->completed= true;
            is_ready        = true;
          }
        }
      }

      if (is_ready) {
        completed_requests.push_back (*it);
        it= g_async_requests.erase (it);
      }
      else {
        ++it;
      }
    }
  }

  // Execute callbacks for completed requests (outside the lock)
  for (auto& req : completed_requests) {
    gf::pointer ht= response2hashtable (req->sc, req->response);
    gf::call (req->sc, req->callback, gf::cons (req->sc, ht, gf::nil (req->sc)));
    gf::gc_unprotect_at (req->sc, req->gc_loc);
  }

  return static_cast<int> (completed_requests.size ());
}

// Start an async HTTP GET request
static gf::pointer
f_http_async_get (gf::scheme* sc, gf::pointer args) {
  const char* url     = gf::string (gf::car (args));
  gf::pointer  params  = gf::cadr (args);
  gf::pointer  headers = gf::caddr (args);
  gf::pointer  proxy   = gf::cadddr (args);
  gf::pointer  callback= gf::car (gf::cddddr (args));

  if (!gf::is_procedure (callback)) {
    return gf::error (sc, gf::make_symbol (sc, "type-error"),
                     gf::list (sc, gf::make_string (sc, "http-async-get: callback must be a procedure"), callback));
  }

  cpr::Parameters cpr_params = to_cpr_parameters (sc, params);
  cpr::Header     cpr_headers= to_cpr_headers (sc, headers);
  cpr::Proxies    cpr_proxies= to_cpr_proxies (sc, proxy);

  // Protect callback from GC
  int gc_loc= gf::gc_protect (sc, callback);

  // Create session on heap with shared_ptr to keep it alive
  auto session= std::make_shared<cpr::Session> ();
  session->SetUrl (cpr::Url (url));
  session->SetParameters (cpr_params);
  session->SetHeader (cpr_headers);
  if (gf::is_list (sc, proxy) && !gf::is_null (sc, proxy)) {
    session->SetProxies (cpr_proxies);
  }

  // Start async request using libcpr's built-in thread pool
  // Session is captured by shared_ptr, so it stays alive until async operation completes
  auto async_resp= session->GetAsync ();

  // Store the request (session is also stored to keep reference)
  auto req= std::make_shared<AsyncHttpRequest> (sc, callback, gc_loc, session, std::move (async_resp));
  {
    std::lock_guard<std::mutex> lock (g_async_requests_mutex);
    g_async_requests.push_back (req);
  }

  return gf::make_boolean (sc, true);
}

inline void
glue_http_async_get (gf::scheme* sc) {
  gf::pointer  cur_env= gf::curlet (sc);
  const char* name   = "g_http-async-get";
  const char* doc = "(g_http-async-get url params headers proxy callback) => boolean, start async http get. callback "
                    "receives response hashtable. Use g_http-poll to check for completion.";
  auto        func= gf::make_typed_function (sc, name, f_http_async_get, 5, 0, false, doc, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, name), func);
}

// Start an async HTTP POST request
static gf::pointer
f_http_async_post (gf::scheme* sc, gf::pointer args) {
  const char* url     = gf::string (gf::car (args));
  gf::pointer  params  = gf::cadr (args);
  const char* body    = gf::string (gf::caddr (args));
  gf::pointer  headers = gf::cadddr (args);
  gf::pointer  proxy   = gf::car (gf::cddddr (args));
  gf::pointer  callback= gf::cadr (gf::cddddr (args));

  if (!gf::is_procedure (callback)) {
    return gf::error (sc, gf::make_symbol (sc, "type-error"),
                     gf::list (sc, gf::make_string (sc, "http-async-post: callback must be a procedure"), callback));
  }

  cpr::Parameters cpr_params = to_cpr_parameters (sc, params);
  cpr::Header     cpr_headers= to_cpr_headers (sc, headers);
  cpr::Proxies    cpr_proxies= to_cpr_proxies (sc, proxy);

  // Protect callback from GC
  int gc_loc= gf::gc_protect (sc, callback);

  // Create session on heap with shared_ptr to keep it alive
  auto session= std::make_shared<cpr::Session> ();
  session->SetUrl (cpr::Url (url));
  session->SetParameters (cpr_params);
  session->SetBody (cpr::Body (body));
  session->SetHeader (cpr_headers);
  if (gf::is_list (sc, proxy) && !gf::is_null (sc, proxy)) {
    session->SetProxies (cpr_proxies);
  }

  // Start async request using libcpr's built-in thread pool
  auto async_resp= session->PostAsync ();

  // Store the request (session is also stored to keep reference)
  auto req= std::make_shared<AsyncHttpRequest> (sc, callback, gc_loc, session, std::move (async_resp));
  {
    std::lock_guard<std::mutex> lock (g_async_requests_mutex);
    g_async_requests.push_back (req);
  }

  return gf::make_boolean (sc, true);
}

inline void
glue_http_async_post (gf::scheme* sc) {
  gf::pointer  cur_env= gf::curlet (sc);
  const char* name   = "g_http-async-post";
  const char* doc    = "(g_http-async-post url params body headers proxy callback) => boolean, start async http post. "
                       "callback receives response hashtable. Use g_http-poll to check for completion.";
  auto        func   = gf::make_typed_function (sc, name, f_http_async_post, 6, 0, false, doc, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, name), func);
}

// Start an async HTTP HEAD request
static gf::pointer
f_http_async_head (gf::scheme* sc, gf::pointer args) {
  const char* url     = gf::string (gf::car (args));
  gf::pointer  params  = gf::cadr (args);
  gf::pointer  headers = gf::caddr (args);
  gf::pointer  proxy   = gf::cadddr (args);
  gf::pointer  callback= gf::car (gf::cddddr (args));

  if (!gf::is_procedure (callback)) {
    return gf::error (sc, gf::make_symbol (sc, "type-error"),
                     gf::list (sc, gf::make_string (sc, "http-async-head: callback must be a procedure"), callback));
  }

  cpr::Parameters cpr_params = to_cpr_parameters (sc, params);
  cpr::Header     cpr_headers= to_cpr_headers (sc, headers);
  cpr::Proxies    cpr_proxies= to_cpr_proxies (sc, proxy);

  // Protect callback from GC
  int gc_loc= gf::gc_protect (sc, callback);

  // Create session on heap with shared_ptr to keep it alive
  auto session= std::make_shared<cpr::Session> ();
  session->SetUrl (cpr::Url (url));
  session->SetParameters (cpr_params);
  session->SetHeader (cpr_headers);
  if (gf::is_list (sc, proxy) && !gf::is_null (sc, proxy)) {
    session->SetProxies (cpr_proxies);
  }

  // Start async request using libcpr's built-in thread pool
  auto async_resp= session->HeadAsync ();

  // Store the request (session is also stored to keep reference)
  auto req= std::make_shared<AsyncHttpRequest> (sc, callback, gc_loc, session, std::move (async_resp));
  {
    std::lock_guard<std::mutex> lock (g_async_requests_mutex);
    g_async_requests.push_back (req);
  }

  return gf::make_boolean (sc, true);
}

inline void
glue_http_async_head (gf::scheme* sc) {
  gf::pointer  cur_env= gf::curlet (sc);
  const char* name   = "g_http-async-head";
  const char* doc = "(g_http-async-head url params headers proxy callback) => boolean, start async http head. callback "
                    "receives response hashtable. Use g_http-poll to check for completion.";
  auto        func= gf::make_typed_function (sc, name, f_http_async_head, 5, 0, false, doc, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, name), func);
}

// Poll for completed async HTTP requests and execute their callbacks
static gf::pointer
f_http_poll (gf::scheme* sc, gf::pointer args) {
  int executed= process_async_http_callbacks ();
  return gf::make_integer (sc, executed);
}

inline void
glue_http_poll (gf::scheme* sc) {
  gf::pointer  cur_env= gf::curlet (sc);
  const char* name   = "g_http-poll";
  const char* doc    = "(g_http-poll) => integer, check for completed async http requests and execute their callbacks. "
                       "Returns number of callbacks executed.";
  auto        func   = gf::make_typed_function (sc, name, f_http_poll, 0, 0, false, doc, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, name), func);
}

// Wait for all pending async HTTP requests to complete (blocking)
static gf::pointer
f_http_wait_all (gf::scheme* sc, gf::pointer args) {
  gf::double_ timeout_sec= -1.0; // -1 means wait forever
  if (gf::is_real (gf::car (args))) {
    timeout_sec= gf::real (gf::car (args));
  }

  auto start         = std::chrono::steady_clock::now ();
  bool has_pending   = true;
  int  total_executed= 0;

  while (has_pending) {
    int executed= process_async_http_callbacks ();
    total_executed+= executed;

    // Check if there are still pending requests
    {
      std::lock_guard<std::mutex> lock (g_async_requests_mutex);
      has_pending= !g_async_requests.empty ();
    }

    if (has_pending) {
      // Check timeout
      if (timeout_sec >= 0) {
        auto elapsed=
            std::chrono::duration_cast<std::chrono::milliseconds> (std::chrono::steady_clock::now () - start).count () /
            1000.0;
        if (elapsed >= timeout_sec) {
          break; // Timeout
        }
      }
      // Small sleep to avoid busy waiting
      std::this_thread::sleep_for (std::chrono::milliseconds (10));
    }
  }

  return gf::make_integer (sc, total_executed);
}

inline void
glue_http_wait_all (gf::scheme* sc) {
  gf::pointer  cur_env= gf::curlet (sc);
  const char* name   = "g_http-wait-all";
  const char* doc    = "(g_http-wait-all [timeout-seconds]) => integer, wait for all pending async http requests to "
                       "complete. timeout < 0 means wait forever. Returns number of callbacks executed.";
  auto        func   = gf::make_typed_function (sc, name, f_http_wait_all, 0, 1, false, doc, NULL);
  gf::define (sc, cur_env, gf::make_symbol (sc, name), func);
}

void
glue_http_async (gf::scheme* sc) {
  glue_http_async_get (sc);
  glue_http_async_post (sc);
  glue_http_async_head (sc);
  glue_http_poll (sc);
  glue_http_wait_all (sc);
}

} // namespace goldfish
