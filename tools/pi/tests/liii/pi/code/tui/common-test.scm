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
  (liii fs-watch)
  (liii pi code tui common)
) ;import

(check-set-mode! 'report)

;; 1. 测试 %keyword-value
(check (%keyword-value :key '(:key "val") #f) => "val")
(check (%keyword-value :missing '(:key "val") "default") => "default")
(check (%keyword-value :key '() "default") => "default")

;; 2. 测试 detect-git-branch 返回合法分支名或 #f（在 detached HEAD / CI 环境下为 #f）
(let ((branch (detect-git-branch)))
  (check-true (or (not branch) (string? branch)))
  (when (string? branch)
    (check-true (not (string-null? branch)))
  ) ;when
) ;let

;; 3. detect-git-dir：返回 .git 目录绝对路径或 #f（非 git 仓库 / git 缺失）
(let ((d (detect-git-dir)))
  (check-true (or (not d) (and (string? d) (not (string-null? d)))))
  (when d
    (check-true (string-contains? d ".git"))
  ) ;when
) ;let

;; 4. make-branch-watcher 参数校验：非法 git-dir 优雅降级为 #f；stop 幂等且容忍 #f
(check-false (make-branch-watcher (make-chan 4) #f))
(check-false (make-branch-watcher (make-chan 4) "Z:/no/such/dir/.git"))
(check-false (stop-branch-watcher! #f))
(check-false (stop-branch-watcher! "not-observer"))

;; 5. 端到端：临时 git 仓库内切分支 → 收到 branch_changed 信号
;;    （真实 C watcher 线程 + (liii go) worker 独立 s7 会话回调 + channel 跨会话桥接）
;; %dbg 显式 flush：子进程 stdout 在捕获管道下全缓冲，
;; 必须强制刷新才能在 CI 上定位真实卡点
(define (%dbg msg)
  (display msg)
  (newline)
  (flush-output-port (current-output-port))
) ;define

(let ((git? (either-right? (run-either '("git" "--version")
                            :stdout 'discard :stderr 'discard :timeout 10))))
  (when git?
    (%dbg "[dbg5] begin")
    (let* ((dir (make-temp-dir "gf-pi-gitwatch-"))
           (ch (make-chan 16))
          ) ;
      (check-true
        (either-right?
          (run-either (list "git" "init" dir)
            :stdout 'discard :stderr 'discard :timeout 30)))
      (%dbg "[dbg5] git init done")
      (let ((obs (make-branch-watcher ch (string-append dir "/.git"))))
        (check-true (fs-observer? obs))
        (%dbg "[dbg5] watcher started")
        ;; 切换分支重写 .git/HEAD（rename 或原地改写），应触发 branch_changed
        (check-true
          (either-right?
            (run-either (list "git" "-C" dir "checkout" "-b" "feature-x")
              :stdout 'discard :stderr 'discard :timeout 30)))
        (%dbg "[dbg5] checkout done")
        (let ((got
                (let ((deadline (+ (current-second) 15)))
                  (let loop
                    ((n 0))
                    (when (zero? (modulo n 20))
                      (%dbg (string-append "[dbg5]   poll iter=" (object->string n))))
                    (let ((v (chan-try-recv! ch #f)))
                      (cond (v v)
                            ((> (current-second) deadline) #f)
                            (else (sleep 0.05) (loop (+ n 1)))
                      ) ;cond
                    ) ;let
                  ) ;let loop
                ) ;let
              ) ;got
           ) ;
          (%dbg "[dbg5] got event")
          (check got => '(branch_changed))
        ) ;let
        ;; 停止幂等
        (stop-branch-watcher! obs)
        (stop-branch-watcher! obs)
        (%dbg "[dbg5] stopped")
      ) ;let
      (g_rmtree dir)
      (%dbg "[dbg5] rmtree done")
    ) ;let*
  ) ;when
) ;let

(%dbg "[dbg5] ALL-DONE")

(check-report)
