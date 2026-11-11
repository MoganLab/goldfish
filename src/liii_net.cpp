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

#include "liii_net.hpp"
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

// 单次 net-conn-read 读取的上限：错误路径 s7_error 的 longjmp 不可跨越
// 带析构的 C++ 局部对象（MSVC 下栈展开会被破坏），故使用固定栈缓冲而非 std::vector。
#define GOLDFISH_NET_SMALL_BUF 4096
#define GOLDFISH_NET_MAX_READ 16777216

#if defined(_WIN32) || defined(_WIN64)
#define GOLDFISH_NET_WINDOWS 1
#include <winsock2.h>
#include <ws2tcpip.h>
#else
#define GOLDFISH_NET_POSIX 1
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#endif

namespace goldfish {

// 与 liii_os/liii_subprocess 一致：safe typed function，
// C 侧抛错（s7_error/longjmp）时不会留下未清的 gc-protect 栈帧
inline void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

inline void
glue_define_rest (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, 0, true, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

#if defined(GOLDFISH_NET_WINDOWS)
using socket_t                          = SOCKET;
static const socket_t INVALID_SOCKET_VAL= INVALID_SOCKET;

static void
init_winsock () {
  static std::once_flag init_flag;
  std::call_once (init_flag, [] () {
    WSADATA wsa_data;
    WSAStartup (MAKEWORD (2, 2), &wsa_data);
  });
}

static void
close_socket_fd (socket_t fd) {
  if (fd != INVALID_SOCKET) {
    closesocket (fd);
  }
}

static int
get_last_net_error () {
  return WSAGetLastError ();
}

// 错误消息填充到调用方提供的栈缓冲，避免返回 std::string：
// s7_error 的 longjmp 不允许调用链上存在存活的 C++ 析构对象（MSVC 栈展开会被破坏）。
static void
get_last_net_error_message (int code, char* out, size_t out_size) {
  if (!out || out_size == 0) return;
  out[0]     = '\0';
  char* buf  = nullptr;
  DWORD flags= FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS;
  DWORD size=
      FormatMessageA (flags, nullptr, code, MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR) &buf, 0, nullptr);
  if (size > 0 && buf) {
    while (size > 0 && (buf[size - 1] == '\r' || buf[size - 1] == '\n' || buf[size - 1] == ' ')) {
      buf[--size]= '\0';
    }
    snprintf (out, out_size, "%s", buf);
    LocalFree (buf);
  }
  else {
    snprintf (out, out_size, "Unknown socket error (%d)", code);
  }
}
#else
using socket_t                          = int;
static const socket_t INVALID_SOCKET_VAL= -1;

static void
init_winsock () {}

static void
close_socket_fd (socket_t fd) {
  if (fd >= 0) {
    ::close (fd);
  }
}

static int
get_last_net_error () {
  return errno;
}

static void
get_last_net_error_message (int code, char* out, size_t out_size) {
  if (!out || out_size == 0) return;
  const char* s= strerror (code);
  if (s) snprintf (out, out_size, "%s", s);
  else snprintf (out, out_size, "Unknown socket error (%d)", code);
}
#endif

struct NetListener {
  socket_t           fd{INVALID_SOCKET_VAL};
  int                port{0};
  std::string        addr;
  bool               closed{false};
  mutable std::mutex mtx;

  void close () {
    std::lock_guard<std::mutex> lock (mtx);
    if (!closed) {
      if (fd != INVALID_SOCKET_VAL) {
        close_socket_fd (fd);
        fd= INVALID_SOCKET_VAL;
      }
      closed= true;
    }
  }

  ~NetListener () { close (); }
};

struct NetConn {
  socket_t           fd{INVALID_SOCKET_VAL};
  std::string        peer_addr;
  bool               closed{false};
  mutable std::mutex mtx;

  void close () {
    std::lock_guard<std::mutex> lock (mtx);
    if (!closed) {
      if (fd != INVALID_SOCKET_VAL) {
        close_socket_fd (fd);
        fd= INVALID_SOCKET_VAL;
      }
      closed= true;
    }
  }

  ~NetConn () { close (); }
};

static std::mutex                             g_net_type_mtx;
static std::unordered_map<s7_scheme*, s7_int> g_net_listener_tags;
static std::unordered_map<s7_scheme*, s7_int> g_net_conn_tags;

static s7_int
get_net_listener_tag (s7_scheme* sc) {
  std::lock_guard<std::mutex> lock (g_net_type_mtx);
  auto                        it= g_net_listener_tags.find (sc);
  return (it != g_net_listener_tags.end ()) ? it->second : 0;
}

static s7_int
get_net_conn_tag (s7_scheme* sc) {
  std::lock_guard<std::mutex> lock (g_net_type_mtx);
  auto                        it= g_net_conn_tags.find (sc);
  return (it != g_net_conn_tags.end ()) ? it->second : 0;
}

static bool
is_net_listener (s7_scheme* sc, s7_pointer obj) {
  return s7_is_c_object (obj) && s7_c_object_type (obj) == get_net_listener_tag (sc);
}

static bool
is_net_conn (s7_scheme* sc, s7_pointer obj) {
  return s7_is_c_object (obj) && s7_c_object_type (obj) == get_net_conn_tag (sc);
}

static NetListener*
get_net_listener (s7_scheme* sc, s7_pointer obj) {
  return static_cast<NetListener*> (s7_c_object_value (obj));
}

static NetConn*
get_net_conn (s7_scheme* sc, s7_pointer obj) {
  return static_cast<NetConn*> (s7_c_object_value (obj));
}

static void
net_listener_free (void* val) {
  if (val != nullptr) {
    auto* ln= static_cast<NetListener*> (val);
    delete ln;
  }
}

static void
net_conn_free (void* val) {
  if (val != nullptr) {
    auto* conn= static_cast<NetConn*> (val);
    delete conn;
  }
}

static s7_pointer
net_listener_to_string (s7_scheme* sc, s7_pointer args) {
  s7_pointer self= s7_car (args);
  auto*      ln  = get_net_listener (sc, self);
  if (!ln) {
    return s7_make_string (sc, "#<net-listener: invalid>");
  }
  std::lock_guard<std::mutex> lock (ln->mtx);
  std::ostringstream          oss;
  oss << "#<net-listener " << (ln->closed ? "closed" : ln->addr) << ">";
  return s7_make_string (sc, oss.str ().c_str ());
}

static s7_pointer
net_conn_to_string (s7_scheme* sc, s7_pointer args) {
  s7_pointer self= s7_car (args);
  auto*      conn= get_net_conn (sc, self);
  if (!conn) {
    return s7_make_string (sc, "#<net-conn: invalid>");
  }
  std::lock_guard<std::mutex> lock (conn->mtx);
  std::ostringstream          oss;
  oss << "#<net-conn " << (conn->closed ? "closed" : ("peer: " + conn->peer_addr)) << ">";
  return s7_make_string (sc, oss.str ().c_str ());
}

static s7_pointer
raise_net_type_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "type-error"),
                   s7_list (sc, 2, s7_make_string (sc, msg), arg ? arg : s7_nil (sc)));
}

static s7_pointer
raise_net_value_error (s7_scheme* sc, const char* msg, s7_pointer arg) {
  return s7_error (sc, s7_make_symbol (sc, "value-error"),
                   s7_list (sc, 2, s7_make_string (sc, msg), arg ? arg : s7_nil (sc)));
}

// 注意：本函数及其调用链上不得存在存活的 C++ 析构对象，
// 因为 s7_error 以 longjmp 跳出，跨析构对象的 longjmp 在 MSVC 下会破坏栈展开。
static s7_pointer
raise_net_error (s7_scheme* sc, const char* caller, int err_code, const char* msg, s7_pointer extra= nullptr) {
  char full_msg[512];
  snprintf (full_msg, sizeof (full_msg), "%s: %s (errno %d)", caller, msg ? msg : "", err_code);
  s7_pointer details= s7_list (sc, 4, s7_make_string (sc, full_msg), s7_make_string (sc, caller),
                               s7_make_integer (sc, err_code), extra ? extra : s7_nil (sc));
  return s7_error (sc, s7_make_symbol (sc, "net-error"), details);
}

static bool
parse_host_port (const std::string& addr_str, std::string& host, int& port) {
  size_t colon_pos= addr_str.rfind (':');
  if (colon_pos == std::string::npos) {
    return false;
  }
  host                = addr_str.substr (0, colon_pos);
  std::string port_str= addr_str.substr (colon_pos + 1);
  if (port_str.empty ()) return false;
  char* endptr= nullptr;
  long  val   = strtol (port_str.c_str (), &endptr, 10);
  if (*endptr != '\0' || val < 0 || val > 65535) {
    return false;
  }
  port= static_cast<int> (val);
  return true;
}

// 实际监听建立：不抛错（不做 s7_error），错误以负值返回并回填 err_msg。
// 原因：入口函数帧上存在 std::string 等析构对象时，s7_error 的 longjmp
// 跨 C++ 帧会破坏 MSVC 的栈展开，因此 socket 工作全部下沉到本辅助函数。
static int
net_listen_raw (const char* host, int port, socket_t& out_fd, int& out_port, char* out_ip, size_t ip_size) {
  socket_t fd= ::socket (AF_INET, SOCK_STREAM, 0);
  if (fd == INVALID_SOCKET_VAL) return get_last_net_error ();

  int opt= 1;
#if defined(GOLDFISH_NET_WINDOWS)
  setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, (const char*) &opt, sizeof (opt));
#else
  setsockopt (fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof (opt));
#endif

  sockaddr_in sin;
  std::memset (&sin, 0, sizeof (sin));
  sin.sin_family= AF_INET;
  sin.sin_port  = htons (static_cast<uint16_t> (port));

  if (host[0] == '\0' || strcmp (host, "0.0.0.0") == 0) {
    sin.sin_addr.s_addr= htonl (INADDR_ANY);
  }
  else if (strcmp (host, "127.0.0.1") == 0 || strcmp (host, "localhost") == 0) {
    sin.sin_addr.s_addr= htonl (INADDR_LOOPBACK);
  }
  else if (inet_pton (AF_INET, host, &sin.sin_addr) <= 0) {
    close_socket_fd (fd);
    return -2; // invalid IPv4
  }

  if (::bind (fd, (sockaddr*) &sin, sizeof (sin)) != 0) {
    int err= get_last_net_error ();
    close_socket_fd (fd);
    return err;
  }
  if (::listen (fd, 128) != 0) {
    int err= get_last_net_error ();
    close_socket_fd (fd);
    return err;
  }

  sockaddr_in bound_addr;
#if defined(GOLDFISH_NET_WINDOWS)
  int bound_len= sizeof (bound_addr);
#else
  socklen_t bound_len= sizeof (bound_addr);
#endif
  std::memset (&bound_addr, 0, sizeof (bound_addr));
  if (getsockname (fd, (sockaddr*) &bound_addr, &bound_len) != 0) {
    int err= get_last_net_error ();
    close_socket_fd (fd);
    return err;
  }

  out_port= ntohs (bound_addr.sin_port);
  if (out_ip && ip_size > 0) {
    inet_ntop (AF_INET, &bound_addr.sin_addr, out_ip, (socklen_t) ip_size);
  }
  out_fd= fd;
  return 0;
}

// (g_net-listen network address)
static s7_pointer
f_net_listen (s7_scheme* sc, s7_pointer args) {
  init_winsock ();

  s7_pointer net_arg = s7_car (args);
  s7_pointer addr_arg= s7_cadr (args);

  const char* network= nullptr;
  if (s7_is_string (net_arg)) {
    network= s7_string (net_arg);
  }
  else if (s7_is_symbol (net_arg)) {
    network= s7_symbol_name (net_arg);
  }
  else {
    return raise_net_type_error (sc, "g_net-listen: network must be a string or symbol", net_arg);
  }

  if (strcmp (network, "tcp") != 0 && strcmp (network, "tcp4") != 0) {
    return raise_net_value_error (sc, "g_net-listen: only 'tcp' network is supported", net_arg);
  }

  if (!s7_is_string (addr_arg)) {
    return raise_net_type_error (sc, "g_net-listen: address must be a string", addr_arg);
  }

  const char* addr_str= s7_string (addr_arg);
  // host:port 解析仅使用栈缓冲，避免在抛错帧上构造 std::string
  char host_buf[256];
  long port= -1;
  {
    const char* colon= strrchr (addr_str, ':');
    if (!colon) {
      return raise_net_value_error (sc, "g_net-listen: invalid address format, expected host:port", addr_arg);
    }
    size_t host_len= (size_t) (colon - addr_str);
    if (host_len >= sizeof (host_buf)) {
      return raise_net_value_error (sc, "g_net-listen: invalid address format, expected host:port", addr_arg);
    }
    if (host_len > 0) memcpy (host_buf, addr_str, host_len);
    host_buf[host_len]= '\0';

    char* endptr= nullptr;
    port        = strtol (colon + 1, &endptr, 10);
    if (endptr == colon + 1 || *endptr != '\0' || port < 0 || port > 65535) {
      return raise_net_value_error (sc, "g_net-listen: invalid address format, expected host:port", addr_arg);
    }
  }

  socket_t fd                     = INVALID_SOCKET_VAL;
  int      rport                  = 0;
  char     ip_buf[INET_ADDRSTRLEN]= {0};
  int      rc                     = net_listen_raw (host_buf, (int) port, fd, rport, ip_buf, sizeof (ip_buf));
  if (rc == -2) {
    return raise_net_value_error (sc, "g_net-listen: invalid IPv4 address", addr_arg);
  }
  if (rc != 0) {
    char emsg[256];
    get_last_net_error_message (rc, emsg, sizeof (emsg));
    return raise_net_error (sc, "g_net-listen", rc, emsg, addr_arg);
  }

  char addr_buf[INET_ADDRSTRLEN + 8];
  snprintf (addr_buf, sizeof (addr_buf), "%s:%d", ip_buf, rport);

  auto* ln  = new NetListener ();
  ln->fd    = fd;
  ln->port  = rport;
  ln->addr  = addr_buf;
  ln->closed= false;

  return s7_make_c_object (sc, get_net_listener_tag (sc), ln);
}

// (g_net-listener? obj)
static s7_pointer
f_net_listener_p (s7_scheme* sc, s7_pointer args) {
  return s7_make_boolean (sc, is_net_listener (sc, s7_car (args)));
}

// (g_net-listener-port ln)
static s7_pointer
f_net_listener_port (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!is_net_listener (sc, arg)) {
    return raise_net_type_error (sc, "g_net-listener-port: argument must be a net-listener", arg);
  }
  auto* ln= get_net_listener (sc, arg);
  if (!ln) {
    return raise_net_value_error (sc, "g_net-listener-port: invalid listener", arg);
  }
  std::lock_guard<std::mutex> lock (ln->mtx);
  return s7_make_integer (sc, ln->port);
}

// (g_net-listener-addr ln)
static s7_pointer
f_net_listener_addr (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!is_net_listener (sc, arg)) {
    return raise_net_type_error (sc, "g_net-listener-addr: argument must be a net-listener", arg);
  }
  auto* ln= get_net_listener (sc, arg);
  if (!ln) {
    return raise_net_value_error (sc, "g_net-listener-addr: invalid listener", arg);
  }
  std::lock_guard<std::mutex> lock (ln->mtx);
  return s7_make_string (sc, ln->addr.c_str ());
}

// (g_net-listener-close ln)
static s7_pointer
f_net_listener_close (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!is_net_listener (sc, arg)) {
    return raise_net_type_error (sc, "g_net-listener-close: argument must be a net-listener", arg);
  }
  auto* ln= get_net_listener (sc, arg);
  if (ln) {
    ln->close ();
  }
  return s7_unspecified (sc);
}

// 实际 accept：不抛错，错误以负值返回（-1 表示监听器已关闭）
static int
net_accept_raw (NetListener* ln, socket_t& out_fd, char* out_ip, size_t ip_size, int& out_port) {
  socket_t listen_fd= INVALID_SOCKET_VAL;
  {
    std::lock_guard<std::mutex> lock (ln->mtx);
    if (ln->closed || ln->fd == INVALID_SOCKET_VAL) return -1;
    listen_fd= ln->fd;
  }

  sockaddr_in client_addr;
#if defined(GOLDFISH_NET_WINDOWS)
  int client_len= sizeof (client_addr);
#else
  socklen_t client_len= sizeof (client_addr);
#endif
  std::memset (&client_addr, 0, sizeof (client_addr));

  socket_t client_fd= ::accept (listen_fd, (sockaddr*) &client_addr, &client_len);
  if (client_fd == INVALID_SOCKET_VAL) return get_last_net_error ();

  if (out_ip && ip_size > 0) inet_ntop (AF_INET, &client_addr.sin_addr, out_ip, (socklen_t) ip_size);
  out_port= ntohs (client_addr.sin_port);
  out_fd  = client_fd;
  return 0;
}

// (g_net-accept ln)
static s7_pointer
f_net_accept (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!is_net_listener (sc, arg)) {
    return raise_net_type_error (sc, "g_net-accept: argument must be a net-listener", arg);
  }
  auto* ln= get_net_listener (sc, arg);
  if (!ln) {
    return raise_net_value_error (sc, "g_net-accept: invalid listener", arg);
  }

  socket_t cfd                    = INVALID_SOCKET_VAL;
  char     ip_buf[INET_ADDRSTRLEN]= {0};
  int      cport                  = 0;
  int      rc                     = net_accept_raw (ln, cfd, ip_buf, sizeof (ip_buf), cport);
  if (rc == -1) {
    return raise_net_error (sc, "g_net-accept", -1, "listener is closed", arg);
  }
  if (rc != 0) {
    char emsg[256];
    get_last_net_error_message (rc, emsg, sizeof (emsg));
    return raise_net_error (sc, "g_net-accept", rc, emsg, arg);
  }

  auto* conn= new NetConn ();
  char  peer_buf[INET_ADDRSTRLEN + 8];
  snprintf (peer_buf, sizeof (peer_buf), "%s:%d", ip_buf, cport);
  conn->fd       = cfd;
  conn->peer_addr= peer_buf;
  conn->closed   = false;

  return s7_make_c_object (sc, get_net_conn_tag (sc), conn);
}

// (g_net-conn? obj)
static s7_pointer
f_net_conn_p (s7_scheme* sc, s7_pointer args) {
  return s7_make_boolean (sc, is_net_conn (sc, s7_car (args)));
}

// (g_net-conn-peer-addr conn)
static s7_pointer
f_net_conn_peer_addr (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!is_net_conn (sc, arg)) {
    return raise_net_type_error (sc, "g_net-conn-peer-addr: argument must be a net-conn", arg);
  }
  auto* conn= get_net_conn (sc, arg);
  if (!conn) {
    return raise_net_value_error (sc, "g_net-conn-peer-addr: invalid conn", arg);
  }
  std::lock_guard<std::mutex> lock (conn->mtx);
  return s7_make_string (sc, conn->peer_addr.c_str ());
}

// (g_net-conn-close conn)
static s7_pointer
f_net_conn_close (s7_scheme* sc, s7_pointer args) {
  s7_pointer arg= s7_car (args);
  if (!is_net_conn (sc, arg)) {
    return raise_net_type_error (sc, "g_net-conn-close: argument must be a net-conn", arg);
  }
  auto* conn= get_net_conn (sc, arg);
  if (conn) {
    conn->close ();
  }
  return s7_unspecified (sc);
}

// 实际读取：本函数绝不抛错（不做 s7_error），错误以负值返回，
// 避免大栈帧/长缓冲函数内的 longjmp 在 MSVC 下破坏栈展开。
static int
net_recv_raw (socket_t fd, uint8_t* buf, s7_int len) {
#if defined(GOLDFISH_NET_WINDOWS)
  return ::recv (fd, (char*) buf, (int) len, 0);
#else
  return (int) ::recv (fd, buf, (size_t) len, 0);
#endif
}

// (g_net-conn-read conn [max-bytes])
static s7_pointer
f_net_conn_read (s7_scheme* sc, s7_pointer args) {
  s7_pointer conn_arg= s7_car (args);
  if (!is_net_conn (sc, conn_arg)) {
    return raise_net_type_error (sc, "g_net-conn-read: first argument must be a net-conn", conn_arg);
  }

  socket_t cfd      = INVALID_SOCKET_VAL;
  s7_int   max_bytes= 4096;
  if (s7_is_pair (s7_cdr (args))) {
    s7_pointer mb_arg= s7_cadr (args);
    if (!s7_is_integer (mb_arg) || s7_integer (mb_arg) <= 0) {
      return raise_net_type_error (sc, "g_net-conn-read: max-bytes must be a positive integer", mb_arg);
    }
    max_bytes= s7_integer (mb_arg);
  }

  auto* conn= get_net_conn (sc, conn_arg);
  if (!conn) {
    return raise_net_value_error (sc, "g_net-conn-read: invalid conn", conn_arg);
  }

  bool closed= false;
  {
    // 仅取 fd，不在 lock_guard 存活期内抛错（longjmp 跨析构对象帧会崩溃）
    std::lock_guard<std::mutex> lock (conn->mtx);
    closed= (conn->closed || conn->fd == INVALID_SOCKET_VAL);
    cfd   = conn->fd;
  }
  if (closed) {
    return raise_net_error (sc, "g_net-conn-read", -1, "conn is closed", conn_arg);
  }

  if (max_bytes > GOLDFISH_NET_MAX_READ) max_bytes= GOLDFISH_NET_MAX_READ;
  // 读取在独立辅助函数完成，本帧只保留小栈缓冲；辅助函数内不抛错
  uint8_t    small_buf[GOLDFISH_NET_SMALL_BUF];
  s7_pointer bv= s7_f (sc);
  if (max_bytes <= GOLDFISH_NET_SMALL_BUF) {
    int n= net_recv_raw (cfd, small_buf, max_bytes);
    if (n == 0) return s7_eof_object (sc);
    if (n < 0) {
      int  err= get_last_net_error ();
      char emsg[256];
      get_last_net_error_message (err, emsg, sizeof (emsg));
      return raise_net_error (sc, "g_net-conn-read", err, emsg, conn_arg);
    }
    bv= s7_make_byte_vector (sc, n, 1, nullptr);
    std::memcpy (s7_byte_vector_elements (bv), small_buf, (size_t) n);
    return bv;
  }

  // 大读取：先分配 bytevector，直接收进其中；n 不足时收缩拷贝
  bv          = s7_make_byte_vector (sc, max_bytes, 1, nullptr);
  uint8_t* dst= s7_byte_vector_elements (bv);
  s7_int   off= 0;
  while (off < max_bytes) {
    s7_int chunk= max_bytes - off;
    if (chunk > GOLDFISH_NET_SMALL_BUF) chunk= GOLDFISH_NET_SMALL_BUF;
    int n= net_recv_raw (cfd, dst + off, chunk);
    if (n == 0) break; // 对端 EOF
    if (n < 0) {
      int  err= get_last_net_error ();
      char emsg[256];
      get_last_net_error_message (err, emsg, sizeof (emsg));
      return raise_net_error (sc, "g_net-conn-read", err, emsg, conn_arg);
    }
    off+= n;
  }
  if (off == 0) return s7_eof_object (sc);
  if (off == max_bytes) return bv;
  s7_pointer exact= s7_make_byte_vector (sc, off, 1, nullptr);
  std::memcpy (s7_byte_vector_elements (exact), dst, (size_t) off);
  return exact;
}

// (g_net-conn-write conn data)
static s7_pointer
f_net_conn_write (s7_scheme* sc, s7_pointer args) {
  s7_pointer conn_arg= s7_car (args);
  s7_pointer data_arg= s7_cadr (args);

  if (!is_net_conn (sc, conn_arg)) {
    return raise_net_type_error (sc, "g_net-conn-write: first argument must be a net-conn", conn_arg);
  }

  const char* buf_ptr= nullptr;
  size_t      buf_len= 0;

  if (s7_is_string (data_arg)) {
    buf_ptr= s7_string (data_arg);
    buf_len= s7_string_length (data_arg);
  }
  else if (s7_is_byte_vector (data_arg)) {
    buf_ptr= (const char*) s7_byte_vector_elements (data_arg);
    buf_len= (size_t) s7_vector_length (data_arg);
  }
  else {
    return raise_net_type_error (sc, "g_net-conn-write: data must be a string or bytevector", data_arg);
  }

  auto* conn= get_net_conn (sc, conn_arg);
  if (!conn) {
    return raise_net_value_error (sc, "g_net-conn-write: invalid conn", conn_arg);
  }

  socket_t cfd   = INVALID_SOCKET_VAL;
  bool     closed= false;
  {
    // 仅取 fd，不在 lock_guard 存活期内抛错（longjmp 跨析构对象帧会崩溃）
    std::lock_guard<std::mutex> lock (conn->mtx);
    closed= (conn->closed || conn->fd == INVALID_SOCKET_VAL);
    cfd   = conn->fd;
  }
  if (closed) {
    return raise_net_error (sc, "g_net-conn-write", -1, "conn is closed", conn_arg);
  }

  size_t total_sent= 0;
  while (total_sent < buf_len) {
#if defined(GOLDFISH_NET_WINDOWS)
    int n= ::send (cfd, buf_ptr + total_sent, (int) (buf_len - total_sent), 0);
#else
    ssize_t n= ::send (cfd, buf_ptr + total_sent, buf_len - total_sent, 0);
#endif
    if (n <= 0) {
      int  err= get_last_net_error ();
      char emsg[256];
      get_last_net_error_message (err, emsg, sizeof (emsg));
      return raise_net_error (sc, "g_net-conn-write", err, emsg, conn_arg);
    }
    total_sent+= static_cast<size_t> (n);
  }

  return s7_make_integer (sc, static_cast<s7_int> (total_sent));
}

// 实际连接建立：不抛错，错误以负值返回（-2 表示非法 IPv4 地址）
static int
net_dial_raw (const char* host, int port, socket_t& out_fd, char* out_ip, size_t ip_size) {
  socket_t fd= ::socket (AF_INET, SOCK_STREAM, 0);
  if (fd == INVALID_SOCKET_VAL) return get_last_net_error ();

  sockaddr_in sin;
  std::memset (&sin, 0, sizeof (sin));
  sin.sin_family= AF_INET;
  sin.sin_port  = htons (static_cast<uint16_t> (port));

  if (host[0] == '\0' || strcmp (host, "127.0.0.1") == 0 || strcmp (host, "localhost") == 0) {
    sin.sin_addr.s_addr= htonl (INADDR_LOOPBACK);
  }
  else if (inet_pton (AF_INET, host, &sin.sin_addr) <= 0) {
    close_socket_fd (fd);
    return -2;
  }

  if (::connect (fd, (sockaddr*) &sin, sizeof (sin)) != 0) {
    int err= get_last_net_error ();
    close_socket_fd (fd);
    return err;
  }

  if (out_ip && ip_size > 0) {
    inet_ntop (AF_INET, &sin.sin_addr, out_ip, (socklen_t) ip_size);
  }
  out_fd= fd;
  return 0;
}

// (g_net-dial network address)
static s7_pointer
f_net_dial (s7_scheme* sc, s7_pointer args) {
  init_winsock ();

  s7_pointer net_arg = s7_car (args);
  s7_pointer addr_arg= s7_cadr (args);

  const char* network= nullptr;
  if (s7_is_string (net_arg)) {
    network= s7_string (net_arg);
  }
  else if (s7_is_symbol (net_arg)) {
    network= s7_symbol_name (net_arg);
  }
  else {
    return raise_net_type_error (sc, "g_net-dial: network must be a string or symbol", net_arg);
  }

  if (strcmp (network, "tcp") != 0 && strcmp (network, "tcp4") != 0) {
    return raise_net_value_error (sc, "g_net-dial: only 'tcp' network is supported", net_arg);
  }

  if (!s7_is_string (addr_arg)) {
    return raise_net_type_error (sc, "g_net-dial: address must be a string", addr_arg);
  }

  const char* addr_str= s7_string (addr_arg);
  char        host_buf[256];
  long        port= -1;
  {
    const char* colon= strrchr (addr_str, ':');
    if (!colon) {
      return raise_net_value_error (sc, "g_net-dial: invalid address format, expected host:port", addr_arg);
    }
    size_t host_len= (size_t) (colon - addr_str);
    if (host_len >= sizeof (host_buf)) {
      return raise_net_value_error (sc, "g_net-dial: invalid address format, expected host:port", addr_arg);
    }
    if (host_len > 0) memcpy (host_buf, addr_str, host_len);
    host_buf[host_len]= '\0';

    char* endptr= nullptr;
    port        = strtol (colon + 1, &endptr, 10);
    if (endptr == colon + 1 || *endptr != '\0' || port < 0 || port > 65535) {
      return raise_net_value_error (sc, "g_net-dial: invalid address format, expected host:port", addr_arg);
    }
  }

  socket_t fd                     = INVALID_SOCKET_VAL;
  char     ip_buf[INET_ADDRSTRLEN]= {0};
  int      rc                     = net_dial_raw (host_buf, (int) port, fd, ip_buf, sizeof (ip_buf));
  if (rc == -2) {
    return raise_net_value_error (sc, "g_net-dial: invalid IPv4 address", addr_arg);
  }
  if (rc != 0) {
    char emsg[256];
    get_last_net_error_message (rc, emsg, sizeof (emsg));
    return raise_net_error (sc, "g_net-dial", rc, emsg, addr_arg);
  }

  char peer_buf[INET_ADDRSTRLEN + 8];
  snprintf (peer_buf, sizeof (peer_buf), "%s:%d", ip_buf, (int) port);

  auto* conn     = new NetConn ();
  conn->fd       = fd;
  conn->peer_addr= peer_buf;
  conn->closed   = false;

  return s7_make_c_object (sc, get_net_conn_tag (sc), conn);
}

void
glue_liii_net (s7_scheme* sc) {
  init_winsock ();

  s7_int ln_tag= s7_make_c_type (sc, "net_listener");
  s7_c_type_set_free (sc, ln_tag, net_listener_free);
  s7_c_type_set_to_string (sc, ln_tag, net_listener_to_string);

  s7_int conn_tag= s7_make_c_type (sc, "net_conn");
  s7_c_type_set_free (sc, conn_tag, net_conn_free);
  s7_c_type_set_to_string (sc, conn_tag, net_conn_to_string);

  {
    std::lock_guard<std::mutex> lock (g_net_type_mtx);
    g_net_listener_tags[sc]= ln_tag;
    g_net_conn_tags[sc]    = conn_tag;
  }

  glue_define (sc, "g_net-listen", "(g_net-listen network address) => net-listener", f_net_listen, 2, 0);
  glue_define (sc, "g_net-listener?", "(g_net-listener? obj) => boolean", f_net_listener_p, 1, 0);
  glue_define (sc, "g_net-listener-port", "(g_net-listener-port ln) => integer", f_net_listener_port, 1, 0);
  glue_define (sc, "g_net-listener-addr", "(g_net-listener-addr ln) => string", f_net_listener_addr, 1, 0);
  glue_define (sc, "g_net-listener-close", "(g_net-listener-close ln) => unspecified", f_net_listener_close, 1, 0);
  glue_define (sc, "g_net-accept", "(g_net-accept ln) => net-conn", f_net_accept, 1, 0);
  glue_define (sc, "g_net-conn?", "(g_net-conn? obj) => boolean", f_net_conn_p, 1, 0);
  glue_define (sc, "g_net-conn-peer-addr", "(g_net-conn-peer-addr conn) => string", f_net_conn_peer_addr, 1, 0);
  glue_define (sc, "g_net-conn-read", "(g_net-conn-read conn [max-bytes]) => bytevector | eof-object", f_net_conn_read,
               1, 1);
  glue_define (sc, "g_net-conn-write", "(g_net-conn-write conn data) => integer", f_net_conn_write, 2, 0);
  glue_define (sc, "g_net-conn-close", "(g_net-conn-close conn) => unspecified", f_net_conn_close, 1, 0);
  glue_define (sc, "g_net-dial", "(g_net-dial network address) => net-conn", f_net_dial, 2, 0);
}

} // namespace goldfish
