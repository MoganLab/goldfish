;; make-branch-watcher
;; 构造并返回新的 make-branch-watcher 实例组件。
;;
;; 语法
;; ----
;; (make-branch-watcher ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; component?
;; 返回构造的实例。

(import (scheme base)
  (scheme time)
  (liii base)
  (liii check)
  (liii string)
  (liii either)
  (liii time)
  (liii go)
  (liii tempfile)
  (liii subprocess)
  (liii shutil)
  (liii fs-watch)
  (liii pi code modes interactive common)
) ;import

(check-set-mode! 'report)

;; 参数校验：非法 git-dir 优雅降级为 #f；stop 幂等且容忍 #f
(check-false (make-branch-watcher (make-chan 4) #f))
(check-false (make-branch-watcher (make-chan 4) "Z:/no/such/dir/.git"))
(check-false (stop-branch-watcher! #f))
(check-false (stop-branch-watcher! "not-observer"))

;; 端到端：临时 git 仓库内切分支 → 收到 branch_changed 信号
;; （真实 C watcher 线程 + (liii go) worker 独立 s7 会话回调 + channel 跨会话桥接）
;; %dbg 显式 flush：子进程 stdout 在捕获管道下全缓冲，
;; 必须强制刷新才能在 CI 上定位真实卡点

(define (%dbg msg)
  (display msg)
  (newline)
  (flush-output-port (current-output-port))
) ;define

(let ((git? (either-right? (run-either '("git" "--version") :stdout 'discard :stderr 'discard :timeout 10)
            ) ;either-right?
      ) ;git?
     ) ;
  (when git?
    (%dbg "[dbg5] begin")
    (let* ((dir (make-temp-dir "gf-pi-gitwatch-")) (ch (make-chan 16)))
      (check-true (either-right? (run-either (list "git" "init" dir)
                                   :stdout  'discard
                                   :stderr  'discard
                                   :timeout 30
                                 ) ;run-either
                  ) ;either-right?
      ) ;check-true
      (%dbg "[dbg5] git init done")
      (let ((obs (make-branch-watcher ch (string-append dir "/.git"))))
        (check-true (fs-observer? obs))
        (%dbg "[dbg5] watcher started")
        ;; 切换分支重写 .git/HEAD（rename 或原地改写），应触发 branch_changed
        (check-true (either-right? (run-either (list "git" "-C" dir "checkout" "-b" "feature-x")
                                     :stdout  'discard
                                     :stderr  'discard
                                     :timeout 30
                                   ) ;run-either
                    ) ;either-right?
        ) ;check-true
        (%dbg "[dbg5] checkout done")
        (let ((got
                (let ((deadline (+ (current-second) 15)))
                  (let loop
                    ((n 0))
                    (when (zero? (modulo n 20))
                      (%dbg (string-append "[dbg5]   poll iter=" (object->string n)))
                    ) ;when
                    (let ((v (chan-try-recv! ch #f)))
                      (cond (v v)
                            ((> (current-second) deadline) #f)
                            (else (sleep 0.05) (loop (+ n 1)))
                      ) ;cond
                    ) ;let
                  ) ;let
                ) ;let
              ) ;got
             ) ;
          (%dbg "[dbg5] got event")
          (check (car got) => 'branch_changed)
          ;; 事件携带 git-dir，供主会话直接读 HEAD 文件
          (check-true (and (pair? (cdr got)) (string? (cadr got))))
        ) ;let
        ;; 停止幂等
        (stop-branch-watcher! obs)
        (stop-branch-watcher! obs)
        (%dbg "[dbg5] stopped")
      ) ;let
      (rmtree dir #t)
      (%dbg "[dbg5] rmtree done")
    ) ;let*
  ) ;when
) ;let

(%dbg "[dbg5] ALL-DONE")

(check-report)
