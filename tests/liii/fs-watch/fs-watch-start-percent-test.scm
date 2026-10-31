(import (scheme base)
  (scheme time)
  (liii check)
  (liii fs-watch)
  (liii shutil)
  (liii tempfile)
) ;import

(check-set-mode! 'report-failed)

;; fs-watch-start%
;; 带前置校验的底层 watcher 启动包装（C 层 g_fs_watch_start 的安全入口）。
;;
;; 语法
;; ----
;; (fs-watch-start% path recursive) → integer?
;;
;; 参数
;; ----
;; path : string?
;; 要监听的目录路径（须已存在）。
;; recursive : boolean?
;; 是否递归监听子目录；当前版本一律传 #f（递归暂不支持）。
;;
;; 返回值
;; -----
;; integer?
;; watcher 句柄（正整数），供 g_fs_watch_poll / g_fs_watch_stop 使用。
;;
;; 说明
;; ----
;; 1. C 层错误统一返回 #f 哨兵，参数与状态校验在本包装中完成并抛错
;;    （规避 C++ 内 s7_error 跨线程活跃过的栈帧 longjmp 崩溃风险）。
;; 2. start 返回即保证监听就位（Linux/macOS 调用线程同步注册；
;;    Windows 等待监听线程首次 ReadDirectoryChangesW 挂载），此后变更不丢。
;; 3. 一般情况下无需直接使用本函数——通过 make-fs-observer +
;;    fs-observer-schedule! / fs-observer-start! 的高层 API 即可。
;;
;; 错误处理
;; ----
;; path 非字符串或 recursive 非布尔时抛出 type-error；
;; recursive 为 #t（暂不支持）、目录不存在或平台监听初始化失败时抛出 value-error。

(check-catch 'type-error (fs-watch-start% 123 #f))
(check-catch 'type-error (fs-watch-start% "/tmp" "yes"))
(check-catch 'value-error (fs-watch-start% "/tmp" #t))
(check-catch 'value-error (fs-watch-start% "/no/such/dir/gf-fswatch" #f))

;; start 必须在毫秒级返回：Windows 上若句柄没有 FILE_FLAG_OVERLAPPED，
;; ReadDirectoryChangesW 会同步阻塞到目录首次变化，C 层“等待监听就位”
;; 的 2s 超时就会变成固定启动延迟（gf pi 启动即受此影响）。
(let* ((dir (make-temp-dir "gf-fswatch-start-"))
       (t0 (current-second))
       (handle (fs-watch-start% dir #f))
       (elapsed (- (current-second) t0))
      ) ;
  (check-true (integer? handle))
  (check-true (< elapsed 1.0))
  (g_fs_watch_stop handle)
  (rmtree dir #t)
) ;let*

(check-report)
