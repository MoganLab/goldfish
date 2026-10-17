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

#include "liii_signal.hpp"
#include <atomic>
#include <cstring>
#include <vector>

#include <tbox/tbox.h>

#if defined(TB_CONFIG_OS_WINDOWS) || defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <csignal>
#include <windows.h>
#else
#include <csignal>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#endif

namespace goldfish {

static std::atomic<uint64_t> g_pending_signals{0};
static s7_pointer            g_signal_dispatcher= NULL;
static int                   g_dispatcher_gc_loc= -1;
static bool                  g_in_dispatch      = false;

struct SignalDef {
  const char* name;
  int         signum;
};

static const std::vector<SignalDef>&
get_supported_signals () {
  static const std::vector<SignalDef> sigs= {
      {"sigint", SIGINT}, {"sigterm", SIGTERM}, {"sigabrt", SIGABRT},
#if !defined(TB_CONFIG_OS_WINDOWS) && !defined(_WIN32)
      {"sighup", SIGHUP}, {"sigpipe", SIGPIPE}, {"sigusr1", SIGUSR1}, {"sigusr2", SIGUSR2}, {"sigchld", SIGCHLD},
#endif
  };
  return sigs;
}

#if !defined(TB_CONFIG_OS_WINDOWS) && !defined(_WIN32)
static void
posix_signal_handler (int signum) {
  if (signum > 0 && signum < 64) {
    g_pending_signals.fetch_or (1ULL << signum, std::memory_order_relaxed);
  }
}
#else
static void
windows_signal_handler (int signum) {
  if (signum > 0 && signum < 64) {
    g_pending_signals.fetch_or (1ULL << signum, std::memory_order_relaxed);
  }
  signal (signum, windows_signal_handler);
}

static BOOL WINAPI
windows_console_ctrl_handler (DWORD ctrl_type) {
  if (ctrl_type == CTRL_C_EVENT || ctrl_type == CTRL_BREAK_EVENT) {
    g_pending_signals.fetch_or (1ULL << SIGINT, std::memory_order_relaxed);
    return TRUE;
  }
  return FALSE;
}
#endif

enum class signal_action { dfl, ign, trap };

static bool
set_os_signal_action (int signum, signal_action action) {
#if !defined(TB_CONFIG_OS_WINDOWS) && !defined(_WIN32)
  struct sigaction sa;
  std::memset (&sa, 0, sizeof (sa));
  sigemptyset (&sa.sa_mask);

  switch (action) {
  case signal_action::dfl:
    sa.sa_handler= SIG_DFL;
    break;
  case signal_action::ign:
    sa.sa_handler= SIG_IGN;
    break;
  case signal_action::trap:
    sa.sa_handler= posix_signal_handler;
    sa.sa_flags  = SA_RESTART;
    break;
  }
  return sigaction (signum, &sa, NULL) == 0;
#else
  switch (action) {
  case signal_action::dfl:
    signal (signum, SIG_DFL);
    if (signum == SIGINT) {
      SetConsoleCtrlHandler (windows_console_ctrl_handler, FALSE);
    }
    break;
  case signal_action::ign:
    signal (signum, SIG_IGN);
    if (signum == SIGINT) {
      SetConsoleCtrlHandler (windows_console_ctrl_handler, FALSE);
    }
    break;
  case signal_action::trap:
    signal (signum, windows_signal_handler);
    if (signum == SIGINT) {
      SetConsoleCtrlHandler (windows_console_ctrl_handler, TRUE);
    }
    break;
  }
  return true;
#endif
}

static s7_pointer
get_signal_symbol (s7_scheme* sc, int signum) {
  const auto& sigs= get_supported_signals ();
  for (const auto& s : sigs) {
    if (s.signum == signum) {
      return s7_make_symbol (sc, s.name);
    }
  }
  return s7_nil (sc);
}

bool
dispatch_pending_signals (s7_scheme* sc) {
  if (g_pending_signals.load (std::memory_order_relaxed) == 0) {
    return false;
  }
  if (g_in_dispatch || g_signal_dispatcher == NULL) {
    return false;
  }
  g_in_dispatch= true;

  bool dispatched= false;
  while (true) {
    uint64_t current= g_pending_signals.load (std::memory_order_relaxed);
    if (current == 0) break;

    for (int sig= 1; sig < 64; ++sig) {
      uint64_t mask= (1ULL << sig);
      if (current & mask) {
        g_pending_signals.fetch_and (~mask, std::memory_order_relaxed);
        s7_pointer sig_sym= get_signal_symbol (sc, sig);
        if (sig_sym != s7_nil (sc)) {
          dispatched= true;
          s7_call (sc, g_signal_dispatcher, s7_cons (sc, sig_sym, s7_nil (sc)));
        }
      }
    }
  }

  g_in_dispatch= false;
  return dispatched;
}

static void
signal_safe_point_hook (s7_scheme* sc, bool* val) {
  if (g_pending_signals.load (std::memory_order_relaxed) != 0) {
    dispatch_pending_signals (sc);
  }
}

// (g_signal-supported-signals) => ((sigint . 2) (sigterm . 15) ...)
static s7_pointer
f_signal_supported_signals (s7_scheme* sc, s7_pointer args) {
  const auto& sigs  = get_supported_signals ();
  s7_pointer  result= s7_nil (sc);
  for (auto it= sigs.rbegin (); it != sigs.rend (); ++it) {
    s7_pointer entry= s7_cons (sc, s7_make_symbol (sc, it->name), s7_make_integer (sc, it->signum));
    result          = s7_cons (sc, entry, result);
  }
  return result;
}

// (g_signal-os-trap signum mode) => boolean, mode 为 'default / 'ignore / 'catch
static s7_pointer
f_signal_os_trap (s7_scheme* sc, s7_pointer args) {
  int           signum= (int) s7_integer (s7_car (args));
  const char*   mode  = s7_symbol_name (s7_cadr (args));
  signal_action action;
  if (std::strcmp (mode, "default") == 0) {
    action= signal_action::dfl;
  }
  else if (std::strcmp (mode, "ignore") == 0) {
    action= signal_action::ign;
  }
  else if (std::strcmp (mode, "catch") == 0) {
    action= signal_action::trap;
  }
  else {
    return s7_make_boolean (sc, false);
  }
  return s7_make_boolean (sc, set_os_signal_action (signum, action));
}

// (g_signal-raise signum) => unspecified
static s7_pointer
f_signal_raise (s7_scheme* sc, s7_pointer args) {
  int signum= (int) s7_integer (s7_car (args));
  raise (signum);
  dispatch_pending_signals (sc);
  return s7_unspecified (sc);
}

// (g_signal-set-dispatcher proc) => unspecified
static s7_pointer
f_signal_set_dispatcher (s7_scheme* sc, s7_pointer args) {
  s7_pointer proc= s7_car (args);
  if (g_dispatcher_gc_loc != -1) {
    s7_gc_unprotect_at (sc, g_dispatcher_gc_loc);
    g_dispatcher_gc_loc= -1;
  }
  g_signal_dispatcher= proc;
  if (proc != s7_nil (sc)) {
    g_dispatcher_gc_loc= s7_gc_protect (sc, proc);
  }
  return s7_unspecified (sc);
}

// (g_signal-poll) => boolean
static s7_pointer
f_signal_poll (s7_scheme* sc, s7_pointer args) {
  bool ret= dispatch_pending_signals (sc);
  return s7_make_boolean (sc, ret);
}

static void
glue_define (s7_scheme* sc, const char* name, const char* desc, s7_function f, s7_int required, s7_int optional) {
  s7_pointer cur_env= s7_curlet (sc);
  s7_pointer func   = s7_make_typed_function (sc, name, f, required, optional, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

void
glue_liii_signal (s7_scheme* sc) {
  s7_set_begin_hook (sc, signal_safe_point_hook);

  glue_define (sc, "g_signal-supported-signals", "(g_signal-supported-signals) => alist", f_signal_supported_signals, 0,
               0);
  glue_define (sc, "g_signal-os-trap", "(g_signal-os-trap signum mode) => boolean", f_signal_os_trap, 2, 0);
  glue_define (sc, "g_signal-raise", "(g_signal-raise signum) => unspecified", f_signal_raise, 1, 0);
  glue_define (sc, "g_signal-set-dispatcher", "(g_signal-set-dispatcher proc) => unspecified", f_signal_set_dispatcher,
               1, 0);
  glue_define (sc, "g_signal-poll", "(g_signal-poll) => boolean", f_signal_poll, 0, 0);
}

} // namespace goldfish
