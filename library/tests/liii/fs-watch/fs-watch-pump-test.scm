;; fs-watch-pump
;; worker 会话侧的监听泵：循环 poll 事件并分发给对应回调。
;;
;; 语法
;; ----
;; (fs-watch-pump handle cbs-src ctx)
;;
;; 参数
;; ----
;; handle : integer?
;; watcher 句柄，由 fs-watch-start% 创建。
;; cbs-src : list
;; 形如 (('modified . "(lambda (ctx ev) ...)") ...) 的回调源码列表。
;; ctx : any
;; 传给回调的上下文。
;;
;; 返回值
;; ----
;; symbol
;; watcher 停止时返回 'stopped。
;;
;; 说明
;; ----
;; 1. 本函数在 (liii go) worker 会话中执行，
;;    由 fs-observer-start! 以源码形式派发；回调经 fs-watch-make-dispatcher
;;    在 worker 会话重建。
;; 2. 事件由 %fs-event-from-alist 转换为 fs-event 记录后交给
;;    fs-watch-dispatch! 分发。
;;
;; 错误处理
;; ----
;; handle 非法时由 C 层返回错误哨兵，循环随即退出。

(import (scheme base)
  (scheme file)
  (scheme time)
  (liii check)
  (liii string)
  (liii os)
  (liii go)
  (liii tempfile)
  (liii fs-watch)
  (liii shutil)
) ;import

(check-set-mode! 'report-failed)

;; 平台差异：macOS kqueue 后端的事件路径尚未调通，真实事件用例在 macOS 跳过。
(unless (os-macos?)
  (let* ((dir (make-temp-dir "gf-pump-"))
         (handle (fs-watch-start% dir #f))
         (ch (make-chan 8))
         (src '((modified . "(lambda (ctx ev) (chan-send! ctx 'pumped 5000))")))
         (rch
           (go-result (handle src ch)
             (fs-watch-pump handle src ch)
           ) ;go-result
         ) ;rch
        ) ;
    ;; 触发一次目录变更，worker 侧回调经 channel 回传主会话
    (call-with-output-file (string-append dir (string (os-sep)) "probe.txt")
      (lambda (port) (write-string "x" port))
    ) ;call-with-output-file
    (check (chan-recv! ch 10000 #f) => 'pumped)
    ;; 停止 watcher：worker 的 pump 收到 'stopped 后退出
    (check (g_fs_watch_stop handle) => #t)
    (check (go-result-recv! rch 15000) => 'stopped)
    (rmtree dir #t)
  ) ;let*
) ;unless

(check-report)
