(import (scheme base)
  (scheme file)
  (scheme time)
  (liii check)
  (liii string)
  (liii list)
  (liii os)
  (liii time)
  (liii go)
  (liii tempfile)
  (liii fs-watch)
  (liii shutil)
) ;import

(check-set-mode! 'report-failed)

;; (liii fs-watch) 集成测试：真实 watcher + go worker 全链路验证。
;; 各公开函数的文档与单元用例见同目录按函数拆分的测试文件：
;;   make-fs-event-handler-test.scm / fs-event-p-test.scm /
;;   fs-watch-dispatch-bang-test.scm / fs-watch-handler-to-src-list-test.scm /
;;   fs-watch-start-percent-test.scm / observer-test.scm
;; 平台差异：Windows/Linux 提供具体文件事件（created/modified/deleted/moved）；
;; macOS kqueue 后端只报目录级 'modified（path 为监听目录本身）。
;; TODO(macOS): kqueue 事件路径在 macOS CI 上尚未调通（exit 255 待排查），
;; 依赖真实事件的用例暂在 macOS 跳过。

(define %mac? (os-macos?))

(define %test-dir (make-temp-dir "gf-fswatch-"))

(define (%probe-write name content)
  (call-with-output-file (string-append %test-dir (string (os-sep)) name)
    (lambda (port) (write-string content port))
  ) ;call-with-output-file
) ;define

(define %event-log '())

(define (%expect-type platform-type)
  (if %mac? "modified" platform-type)
) ;define

(define (%expect-path-contains filename)
  (if %mac? "gf-fswatch-" filename)
) ;define

(define (%poll-until pred handle . opts)
  ;; 在 deadline 内反复 poll，累积全部事件；直到累积集合满足 pred 或超时
  ;; 返回 (满足时的完整累积列表 . #t) 或 (累积列表 . #f)
  (let ((deadline (+ (current-second) (if (pair? opts) (car opts) 8))))
    (let loop
      ()
      (if (> (current-second) deadline)
        (cons %event-log #f)
        (let ((evs (g_fs_watch_poll handle 500)))
          (cond ((eq? evs 'stopped) (cons %event-log #f))
                ((pair? evs)
                 (set! %event-log (append %event-log evs))
                 (if (pred %event-log) (cons %event-log #t) (loop))
                ) ;
                (else (loop))
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;let
) ;define

;; 4d. args 路径最小复现：cbs-src 经 go-result vars 序列化传入 worker
(let* ((src '((modified . "(lambda (ctx ev) 'ok)")))
       (rch
         (go-result (src)
           (let ((cbs (fs-watch-make-dispatcher src)))
             (procedure? (cdr (assoc 'modified cbs)))
           ) ;let
         ) ;go-result
       ) ;rch
       (r (go-result-recv! rch 10000))
      ) ;
  (check r => #t)
) ;let*

;; ============================================================
;; 三、C 层直连（g_fs_watch_start/poll/stop，主会话直接调用）
;; ============================================================

;; 5. 创建/修改/删除文件产生对应事件；stop 后 poll 返回 'stopped
(unless %mac?
  (let ((handle (fs-watch-start% %test-dir #f)))
    (check-true (integer? handle))
    ;; created：新建探测文件（macOS 上表现为目录级 modified）
    (%probe-write "probe-a.txt" "v1")
    (let ((res
            (%poll-until
              (lambda (events)
                (any
                  (lambda (e)
                    (and (pair? (assoc 'type e))
                      (eq? (cdr (assoc 'type e)) (%expect-type 'created))
                      (string-contains? (cdr (assoc 'path e)) (%expect-path-contains "probe-a.txt"))
                    ) ;and
                  ) ;lambda
                  events
                ) ;any
              ) ;lambda
              handle
            ) ;%poll-until
          ) ;res
         ) ;
      (check-true (cdr res))
    ) ;let
    (set! %event-log '())
    ;; modified：改写同一文件
    (%probe-write "probe-a.txt" "v2")
    (let ((res
            (%poll-until
              (lambda (events)
                (any
                  (lambda (e)
                    (and (pair? (assoc 'type e))
                      (eq? (cdr (assoc 'type e)) (%expect-type 'modified))
                      (string-contains? (cdr (assoc 'path e)) (%expect-path-contains "probe-a.txt"))
                    ) ;and
                  ) ;lambda
                  events
                ) ;any
              ) ;lambda
              handle
            ) ;%poll-until
          ) ;res
         ) ;
      (check-true (cdr res))
    ) ;let
    (set! %event-log '())
    ;; deleted：删除文件
    (remove (string-append %test-dir (string (os-sep)) "probe-a.txt"))
    (let ((res
            (%poll-until
              (lambda (events)
                (any
                  (lambda (e)
                    (and (pair? (assoc 'type e))
                      (eq? (cdr (assoc 'type e)) (%expect-type 'deleted))
                      (string-contains? (cdr (assoc 'path e)) (%expect-path-contains "probe-a.txt"))
                    ) ;and
                  ) ;lambda
                  events
                ) ;any
              ) ;lambda
              handle
            ) ;%poll-until
          ) ;res
         ) ;
      (check-true (cdr res))
    ) ;let
    ;; stop → poll 返回 'stopped；重复 stop 幂等
    (check (g_fs_watch_stop handle) => #t)
    (check (g_fs_watch_poll handle 200) => 'stopped)
    (check (g_fs_watch_stop handle) => #f)
  ) ;let
) ;unless


;; ============================================================
;; 四、Observer 集成（真实 go worker 线程 + chan 桥接）
;; ============================================================

;; 7. 回调在后台线程触发，经 :context chan 桥接回主会话
(unless %mac?
  (let* ((ch (make-chan 32))
         (h
           (make-fs-event-handler
             :on-created
             (lambda (ctx ev) (chan-send! ctx (fs-event-path ev) 5000))
             :on-modified
             (lambda (ctx ev) (chan-send! ctx (fs-event-path ev) 5000))
           ) ;make-fs-event-handler
         ) ;h
         (obs (make-fs-observer))
        ) ;
    (fs-observer-schedule! obs h %test-dir :context ch)
    (fs-observer-start! obs)
    (%probe-write "probe-b.txt" "hello")
    ;; 主会话限时轮询 chan，收到后台回调投递的路径
    (let ((received
            (let ((deadline (+ (current-second) 10)))
              (let loop
                ()
                (if (> (current-second) deadline)
                  #f
                  (let ((v (chan-try-recv! ch #f)))
                    (if v v (begin (sleep 0.05) (loop)))
                  ) ;let
                ) ;if
              ) ;let
            ) ;let
          ) ;received
         ) ;
      (check-true (and received (string? received) (not (string-null? received))))
    ) ;let
    (fs-observer-stop! obs)
    ;; stop 幂等
    (fs-observer-stop! obs)
  ) ;let*
) ;unless


;; 9. C 层多轮启停稳定性：连续 5 轮 start/poll/stop 不崩溃不挂起
(unless %mac?
  (let loop
    ((n 5))
    (when (> n 0)
      (let ((h (fs-watch-start% %test-dir #f)))
        (check-true (integer? h))
        (check (g_fs_watch_poll h 50) => #f)
        (check (g_fs_watch_stop h) => #t)
      ) ;let
      (loop (- n 1))
    ) ;when
  ) ;let
) ;unless

;; 清理
;; 清理（递归删除，容忍事件用例残留的探测文件）
(rmtree %test-dir #t)

(check-report)
