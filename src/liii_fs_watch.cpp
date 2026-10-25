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
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

// 类似 Node.js fs.watch / Python watchdog 的目录监听基础设施：
//   Windows  : ReadDirectoryChangesW
//   Linux    : inotify
//   macOS    : kqueue (EVFILT_VNODE, NOTE_WRITE)
// 事件经互斥队列缓冲，由任意 s7 会话通过 g_fs_watch_poll 消费，
// 因此主会话与 (liii go) worker 会话均可安全调用。

#include "s7.h"

#include <algorithm>
#include <atomic>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <filesystem>
#include <memory>
#include <mutex>
#include <string>
#include <thread>
#include <unordered_map>
#include <vector>

#if defined(_WIN32)
#include <windows.h>
#elif defined(__APPLE__)
#include <fcntl.h>
#include <sys/event.h>
#include <unistd.h>
#else
#include <fcntl.h>
#include <poll.h>
#include <sys/inotify.h>
#include <unistd.h>
#endif

namespace goldfish {

namespace fs= std::filesystem;

// ---------------------------------------------------------------------------
// 事件与 Watcher 对象
// ---------------------------------------------------------------------------

struct FsEvent {
  std::string type; // "modified" / "created" / "deleted" / "moved"
  std::string path; // 事件目标的绝对路径（相对 watch 目录拼接）
  std::string dest; // 仅 "moved"：目标路径；其余为空
};

struct FsWatcher {
  int                     id= 0;
  std::string             watch_dir;
  std::thread             th;
  std::mutex              mtx;
  std::condition_variable cv;
  std::deque<FsEvent>     queue;
  std::atomic<bool>       stopping{false};

#if defined(_WIN32)
  HANDLE dir_handle = INVALID_HANDLE_VALUE;
  HANDLE ready_event= NULL; // 首次 ReadDirectoryChangesW 就位后置位
  // IO 缓冲必须存活至 CloseHandle 之后：CancelIoEx 取消瞬间内核可能
  // 仍向缓冲写入（取消完成有竞态窗口），栈缓冲会触发 fastfail。
  // 挂在 watcher 对象上，析构晚于 CloseHandle，安全。
  std::vector<char> buf{std::vector<char> (64 * 1024)};
#elif defined(__APPLE__)
  int kq_fd = -1;
  int dir_fd= -1;
#else
  int inotify_fd= -1;
  int inotify_wd= -1;
#endif

  void push (FsEvent ev) {
    {
      std::lock_guard<std::mutex> lock (mtx);
      queue.push_back (std::move (ev));
    }
    cv.notify_one ();
  }
};

static std::mutex                                          g_fs_watch_mtx;
static std::unordered_map<int, std::shared_ptr<FsWatcher>> g_fs_watchers;
static std::atomic<int>                                    g_fs_watch_next_id{1};

// ---------------------------------------------------------------------------
// 跨平台辅助
// ---------------------------------------------------------------------------

#if defined(_WIN32)

static std::wstring
fs_utf8_to_wide (const std::string& s) {
  int          n= MultiByteToWideChar (CP_UTF8, 0, s.c_str (), (int) s.size (), nullptr, 0);
  std::wstring w ((size_t) n, L'\0');
  MultiByteToWideChar (CP_UTF8, 0, s.c_str (), (int) s.size (), &w[0], n);
  return w;
}

static std::string
fs_wide_to_utf8 (const wchar_t* w, size_t len) {
  int         n= WideCharToMultiByte (CP_UTF8, 0, w, (int) len, nullptr, 0, nullptr, nullptr);
  std::string s ((size_t) n, '\0');
  WideCharToMultiByte (CP_UTF8, 0, w, (int) len, &s[0], n, nullptr, nullptr);
  return s;
}

static void
fs_watch_loop_win (FsWatcher* w) {
  const DWORD filter= FILE_NOTIFY_CHANGE_FILE_NAME | FILE_NOTIFY_CHANGE_DIR_NAME | FILE_NOTIFY_CHANGE_LAST_WRITE |
                      FILE_NOTIFY_CHANGE_SIZE | FILE_NOTIFY_CHANGE_CREATION;
  std::string pending_old;                                        // RENAMED_OLD_NAME 的源路径（配对 NEW_NAME）
  HANDLE      done_event= CreateEventW (NULL, TRUE, FALSE, NULL); // manual-reset
  bool        first     = true;
  while (!w->stopping.load () && done_event != NULL) {
    OVERLAPPED ovl;
    ZeroMemory (&ovl, sizeof (ovl));
    ovl.hEvent = done_event;
    DWORD bytes= 0;
    // OVERLAPPED 异步模式：立即返回，完成（含取消）由事件通知。
    // GetOverlappedResult 返回后内核保证不再访问 buffer/ovl，
    // 避免取消竞态下内核晚写已析构内存的堆破坏。
    BOOL pending=
        ReadDirectoryChangesW (w->dir_handle, w->buf.data (), (DWORD) w->buf.size (), FALSE, // v1 统一不支持 recursive
                               filter, NULL, &ovl, NULL);
    if (first) {
      // 首次调用已挂上监听：通知 start 调用方可安全返回（此后变更不丢）
      first= false;
      SetEvent (w->ready_event);
    }
    if (!pending && GetLastError () != ERROR_IO_PENDING) break;
    if (WaitForSingleObject (done_event, INFINITE) != WAIT_OBJECT_0) break;
    if (!GetOverlappedResult (w->dir_handle, &ovl, &bytes, FALSE)) {
      if (w->stopping.load ()) break; // CancelIoEx 取消：安全退出
      continue;
    }
    if (bytes == 0) continue;
    size_t offset= 0;
    while (offset < bytes) {
      FILE_NOTIFY_INFORMATION* info= reinterpret_cast<FILE_NOTIFY_INFORMATION*> (w->buf.data () + offset);
      std::string              name= fs_wide_to_utf8 (info->FileName, info->FileNameLength / sizeof (wchar_t));
      std::string              full= w->watch_dir + "\\" + name;
      FsEvent                  ev;
      ev.path= full;
      switch (info->Action) {
      case FILE_ACTION_ADDED:
        ev.type= "created";
        w->push (std::move (ev));
        break;
      case FILE_ACTION_REMOVED:
        ev.type= "deleted";
        w->push (std::move (ev));
        break;
      case FILE_ACTION_MODIFIED:
        ev.type= "modified";
        w->push (std::move (ev));
        break;
      case FILE_ACTION_RENAMED_OLD_NAME:
        pending_old= full;
        break;
      case FILE_ACTION_RENAMED_NEW_NAME:
        if (!pending_old.empty ()) {
          ev.type= "moved";
          ev.path= pending_old;
          ev.dest= full;
          pending_old.clear ();
        }
        else {
          ev.type= "created";
        }
        w->push (std::move (ev));
        break;
      default:
        break;
      }
      if (info->NextEntryOffset == 0) break;
      offset+= info->NextEntryOffset;
    }
  }
  if (done_event != NULL) CloseHandle (done_event);
}

#elif defined(__APPLE__)

static void
fs_watch_loop_mac (FsWatcher* w) {
  // EV_SET 已在 start 调用线程同步完成，此处只等待事件
  while (!w->stopping.load ()) {
    struct kevent   changes[1];
    struct timespec timeout;
    timeout.tv_sec = 0;
    timeout.tv_nsec= 500 * 1000 * 1000; // 500ms
    int n          = kevent (w->kq_fd, NULL, 0, changes, 1, &timeout);
    if (n <= 0) continue;
    // kqueue/VNODE 不提供变更文件名（需 FSEvents），以目录级 modified 事件上报，
    // 对“监听 .git 目录感知 HEAD 变化”等场景完全够用
    FsEvent ev;
    ev.type= "modified";
    ev.path= w->watch_dir;
    w->push (std::move (ev));
  }
}

#else // Linux

static void
fs_watch_loop_linux (FsWatcher* w) {
  // inotify_add_watch 已在 start 调用线程同步完成，此处只消费事件
  // moved 事件按 cookie 配对（同一次 rename 的 FROM/TO 共享 cookie）
  std::unordered_map<uint32_t, std::string> pending_moves;
  std::vector<char>                         buf (64 * 1024);
  while (!w->stopping.load ()) {
    struct pollfd pfd;
    pfd.fd    = w->inotify_fd;
    pfd.events= POLLIN;
    int ready = poll (&pfd, 1, 500);
    if (ready <= 0) continue;
    ssize_t n= read (w->inotify_fd, buf.data (), buf.size ());
    if (n <= 0) continue;
    size_t offset= 0;
    while (offset < (size_t) n) {
      const inotify_event* ie= reinterpret_cast<const inotify_event*> (buf.data () + offset);
      // 内核保证 name 在 ie->len 范围内以 NUL 结尾，strlen 即安全
      // （strnlen 需要 POSIX 2008/_GNU_SOURCE，跨发行版不可靠）
      std::string name_str= (ie->len > 0) ? std::string (ie->name) : std::string ();
      std::string full    = w->watch_dir + "/" + name_str;
      FsEvent     ev;
      ev.path= full;
      if (ie->mask & IN_Q_OVERFLOW) {
        // 队列溢出忽略：等价于丢失事件，后续变更仍会被捕获
      }
      else if (ie->mask & (IN_CREATE | IN_MOVED_TO)) {
        auto it= pending_moves.find (ie->cookie);
        if ((ie->mask & IN_MOVED_TO) && ie->cookie != 0 && it != pending_moves.end ()) {
          ev.type= "moved";
          ev.path= it->second;
          ev.dest= full;
          pending_moves.erase (it);
        }
        else {
          ev.type= "created";
        }
        w->push (std::move (ev));
      }
      else if (ie->mask & IN_DELETE) {
        ev.type= "deleted";
        w->push (std::move (ev));
      }
      else if (ie->mask & IN_MOVED_FROM) {
        if (ie->cookie != 0) pending_moves[ie->cookie]= full;
        else {
          ev.type= "deleted";
          w->push (std::move (ev));
        }
      }
      else if (ie->mask & (IN_MODIFY | IN_CLOSE_WRITE)) {
        ev.type= "modified";
        w->push (std::move (ev));
      }
      else if (ie->mask & IN_DELETE_SELF) {
        ev.type= "deleted";
        ev.path= w->watch_dir;
        w->push (std::move (ev));
      }
      offset+= sizeof (inotify_event) + ie->len;
    }
  }
}

#endif

// ---------------------------------------------------------------------------
// Watcher 生命周期
// ---------------------------------------------------------------------------

static s7_pointer
f_fs_watch_start (s7_scheme* sc, s7_pointer args) {
  s7_pointer path_arg= s7_car (args);
  s7_pointer rec_arg = s7_cadr (args);
  // 错误路径统一返回 #f（哨兵）；C++ 内不做 s7_error：
  // watcher 线程活跃过之后跨 C++ 帧的 longjmp 存在崩溃风险，
  // 参数与状态校验由 Scheme 层 (liii fs-watch) 前置完成并抛错。
  if (!s7_is_string (path_arg) || !s7_is_boolean (rec_arg) || rec_arg == s7_t (sc)) {
    return s7_f (sc);
  }
  std::string     path= s7_string (path_arg);
  std::error_code ec;
  if (!fs::is_directory (fs::path (path), ec)) {
    return s7_f (sc);
  }
  // 统一为原生绝对路径
  std::string abs= fs::absolute (fs::path (path), ec).string ();
  if (ec) abs= path;

  auto w= std::make_shared<FsWatcher> ();
#if defined(_WIN32)
  std::wstring wide= fs_utf8_to_wide (abs);
  w->dir_handle=
      CreateFileW (wide.c_str (), FILE_LIST_DIRECTORY, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE, NULL,
                   OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
  if (w->dir_handle == INVALID_HANDLE_VALUE) {
    return s7_f (sc);
  }
  w->ready_event= CreateEventW (NULL, TRUE, FALSE, NULL); // manual-reset
#elif defined(__APPLE__)
  w->dir_fd= open (abs.c_str (), O_EVTONLY);
  if (w->dir_fd < 0) {
    return s7_f (sc);
  }
  w->kq_fd= kqueue ();
  if (w->kq_fd < 0) {
    close (w->dir_fd);
    return s7_f (sc);
  }
  // 同步注册监听：start 返回即保证监听就位，不丢先前变更
  struct kevent kev;
  EV_SET (&kev, w->dir_fd, EVFILT_VNODE, EV_ADD | EV_CLEAR, NOTE_WRITE, 0, NULL);
  kevent (w->kq_fd, &kev, 1, NULL, 0, NULL);
#else
  w->inotify_fd= inotify_init1 (IN_NONBLOCK | IN_CLOEXEC);
  if (w->inotify_fd < 0) {
    return s7_f (sc);
  }
  // 同步注册监听：inotify_add_watch 返回后内核即开始记录该目录变更
  uint32_t mask= IN_CREATE | IN_DELETE | IN_MODIFY | IN_CLOSE_WRITE | IN_MOVED_FROM | IN_MOVED_TO | IN_DELETE_SELF;
  w->inotify_wd= inotify_add_watch (w->inotify_fd, abs.c_str (), mask);
  if (w->inotify_wd < 0) {
    close (w->inotify_fd);
    return s7_f (sc);
  }
#endif
  w->id       = g_fs_watch_next_id.fetch_add (1);
  w->watch_dir= abs;

  {
    std::lock_guard<std::mutex> lock (g_fs_watch_mtx);
    g_fs_watchers[w->id]= w;
  }

#if defined(_WIN32)
  w->th= std::thread (fs_watch_loop_win, w.get ());
  // 等待监听线程首次 ReadDirectoryChangesW 就位（上限 2s），
  // 保证 start 返回后的变更不会被遗漏
  WaitForSingleObject (w->ready_event, 2000);
#elif defined(__APPLE__)
  w->th= std::thread (fs_watch_loop_mac, w.get ());
#else
  w->th= std::thread (fs_watch_loop_linux, w.get ());
#endif
  return s7_make_integer (sc, w->id);
}

static s7_pointer
f_fs_watch_poll (s7_scheme* sc, s7_pointer args) {
  s7_pointer id_arg= s7_car (args);
  s7_pointer to_arg= s7_cadr (args);
  if (!s7_is_integer (id_arg)) {
    return s7_f (sc);
  }
  if (!s7_is_integer (to_arg)) {
    return s7_f (sc);
  }
  std::shared_ptr<FsWatcher> w;
  {
    std::lock_guard<std::mutex> lock (g_fs_watch_mtx);
    auto                        it= g_fs_watchers.find ((int) s7_integer (id_arg));
    if (it == g_fs_watchers.end ()) return s7_make_symbol (sc, "stopped");
    w= it->second; // 拷贝 shared_ptr：即使并发 stop 也保证本调用安全
  }
  auto                 deadline= std::chrono::steady_clock::now () + std::chrono::milliseconds (s7_integer (to_arg));
  std::vector<FsEvent> batch;
  {
    std::unique_lock<std::mutex> lock (w->mtx);
    while (w->queue.empty () && !w->stopping.load ()) {
      if (w->cv.wait_until (lock, deadline) == std::cv_status::timeout) break;
    }
    // 一次性取走当前缓冲的全部事件
    while (!w->queue.empty ()) {
      batch.push_back (std::move (w->queue.front ()));
      w->queue.pop_front ();
    }
  }
  if (batch.empty ()) {
    if (w->stopping.load ()) return s7_make_symbol (sc, "stopped");
    return s7_f (sc);
  }
  // 构造 s7 事件列表：((type . "modified") (path . "...") [(dest . "...")])
  s7_pointer head= s7_nil (sc);
  for (auto it= batch.rbegin (); it != batch.rend (); ++it) {
    s7_pointer ev= s7_nil (sc);
    if (!it->dest.empty ()) {
      ev= s7_cons (sc, s7_cons (sc, s7_make_symbol (sc, "dest"), s7_make_string (sc, it->dest.c_str ())), ev);
    }
    ev  = s7_cons (sc, s7_cons (sc, s7_make_symbol (sc, "path"), s7_make_string (sc, it->path.c_str ())), ev);
    ev  = s7_cons (sc, s7_cons (sc, s7_make_symbol (sc, "type"), s7_make_symbol (sc, it->type.c_str ())), ev);
    head= s7_cons (sc, ev, head);
  }
  return head;
}

static s7_pointer
f_fs_watch_stop (s7_scheme* sc, s7_pointer args) {
  s7_pointer id_arg= s7_car (args);
  if (!s7_is_integer (id_arg)) {
    return s7_f (sc);
  }
  std::shared_ptr<FsWatcher> w;
  {
    std::lock_guard<std::mutex> lock (g_fs_watch_mtx);
    auto                        it= g_fs_watchers.find ((int) s7_integer (id_arg));
    if (it == g_fs_watchers.end ()) return s7_f (sc); // 幂等：未注册句柄
    w= it->second;                                    // map 移除后仍可能存在并发 poll 的引用，安全
    g_fs_watchers.erase (it);
  }
  w->stopping.store (true);
  {
    // 唤醒可能在等待的 poll
    std::lock_guard<std::mutex> lock (w->mtx);
  }
  w->cv.notify_all ();
#if defined(_WIN32)
  // 取消另一线程阻塞中的 ReadDirectoryChangesW（Vista+）
  if (w->dir_handle != INVALID_HANDLE_VALUE) CancelIoEx (w->dir_handle, NULL);
#elif defined(__APPLE__)
  // 关闭 fd 使阻塞中的 kevent 返回
  if (w->kq_fd >= 0) close (w->kq_fd);
  w->kq_fd= -1;
#else
  if (w->inotify_fd >= 0) close (w->inotify_fd);
  w->inotify_fd= -1;
#endif
  if (w->th.joinable ()) w->th.join ();
#if defined(_WIN32)
  if (w->dir_handle != INVALID_HANDLE_VALUE) CloseHandle (w->dir_handle);
  if (w->ready_event != NULL) CloseHandle (w->ready_event);
#elif defined(__APPLE__)
  if (w->dir_fd >= 0) close (w->dir_fd);
#else
  // inotify_fd 已关闭
#endif
  return s7_t (sc);
}

static void
glue_fs_watch_start (s7_scheme* sc) {
  const char* name   = "g_fs_watch_start";
  const char* desc   = "(g_fs_watch_start path recursive) => integer, start watching directory";
  s7_pointer  cur_env= s7_curlet (sc);
  s7_pointer  func   = s7_make_typed_function (sc, name, f_fs_watch_start, 2, 0, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

static void
glue_fs_watch_poll (s7_scheme* sc) {
  const char* name   = "g_fs_watch_poll";
  const char* desc   = "(g_fs_watch_poll handle timeout-ms) => list|false|stopped";
  s7_pointer  cur_env= s7_curlet (sc);
  s7_pointer  func   = s7_make_typed_function (sc, name, f_fs_watch_poll, 2, 0, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

static void
glue_fs_watch_stop (s7_scheme* sc) {
  const char* name   = "g_fs_watch_stop";
  const char* desc   = "(g_fs_watch_stop handle) => boolean, stop watching (idempotent)";
  s7_pointer  cur_env= s7_curlet (sc);
  s7_pointer  func   = s7_make_typed_function (sc, name, f_fs_watch_stop, 1, 0, false, desc, NULL);
  s7_define (sc, cur_env, s7_make_symbol (sc, name), func);
}

void
glue_liii_fs_watch (s7_scheme* sc) {
  glue_fs_watch_start (sc);
  glue_fs_watch_poll (sc);
  glue_fs_watch_stop (sc);
}

} // namespace goldfish
