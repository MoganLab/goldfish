(import (scheme base)
  (scheme file)
  (scheme time)
  (liii check)
  (liii string)
  (liii list)
  (liii os)
  (liii time)
  (liii go)
  (liii fs-watch)
) ;import

(check-set-mode! 'report-failed)

;; 构造本轮测试专用的临时目录（进程级唯一，避免并行冲突）
(define %test-dir
  (string-append (os-temp-dir) (string (os-sep)) "gf-fswatch-"
    (number->string (getpid))
  ) ;string-append
) ;define

(define (%probe-write name content)
  (call-with-output-file (string-append %test-dir (string (os-sep)) name)
    (lambda (port) (write-string content port))
  ) ;call-with-output-file
) ;define

(define %event-log '())   ; 诊断：累积所有收到的事件

;; 平台差异：macOS kqueue 后端只报目录级 "modified"（path 为监听目录），
;; 不提供文件名与 created/deleted/moved 细分；Windows/Linux 报具体文件事件
(define %mac? (os-macos?))

(define (%expect-type platform-type) (if %mac? "modified" platform-type))
(define (%expect-path-contains filename)
  (if %mac? "gf-fswatch-" filename)
) ;define

(define (%poll-until pred handle . opts)
  ;; 在 deadline 内反复 poll，累积全部事件；直到累积集合满足 pred 或超时
  ;; 返回 (满足时的完整累积列表 . #t) 或 (累积列表 . #f)
  (let ((deadline (+ (current-second) (if (pair? opts) (car opts) 8))))
    (let loop ()
      (if (> (current-second) deadline)
        (cons %event-log #f)
        (let ((evs (g_fs_watch_poll handle 500)))
          (cond ((eq? evs 'stopped) (cons %event-log #f))
                ((pair? evs)
                 (set! %event-log (append %event-log evs))
                 (if (pred %event-log)
                   (cons %event-log #t)
                   (loop)
                 ) ;if
                ) ;
                (else (loop))
          ) ;cond
        ) ;let
      ) ;if
    ) ;let
  ) ;let
) ;define

(when (access %test-dir 'F_OK) (rmdir %test-dir))
(mkdir %test-dir)

;; ============================================================
;; 一、handler record（对齐 watchdog FileSystemEventHandler）
;; ============================================================

;; 1. 四个回调槽的存取与缺省 #f
(let ((h (make-fs-event-handler
           :on-modified (lambda (ctx ev) 'mod)
           :on-created  (lambda (ctx ev) 'cre)
         ) ;make-fs-event-handler
      ) ;h
    ) ;
  (check-true (fs-event-handler? h))
  (check-true (procedure? (fs-handler-on-modified h)))
  (check-true (procedure? (fs-handler-on-created h)))
  (check-false (fs-handler-on-deleted h))
  (check-false (fs-handler-on-moved h))
) ;let*

;; 2. 事件 record 访问器（moved 携带 dest，其余为 #f）
(let ((ev (%make-fs-event-for-test "modified" "/x/HEAD" #f)))
  (check (fs-event-type ev) => "modified")
  (check (fs-event-path ev) => "/x/HEAD")
  (check-false (fs-event-dest-path ev))
) ;let

;; ============================================================
;; 二、纯分发逻辑（不依赖真实 watcher）
;; ============================================================

;; 3. dispatch! 按类型调用对应回调，签名 (ctx event)
(let* ((log '())
       (cbs (list (cons "modified"
                     (lambda (ctx ev)
                       (set! log (cons (list ctx (fs-event-type ev) (fs-event-path ev))
                                 log
                               ) ;cons
                       ) ;set!
                     ) ;lambda
                   ) ;cons
                 ) ;list
            ) ;cbs
      ) ;let*
  (fs-watch-dispatch! cbs 'ctx-token (%make-fs-event-for-test "modified" "/a/b" #f))
  (fs-watch-dispatch! cbs 'ctx-token (%make-fs-event-for-test "deleted" "/a/b" #f))
  ;; deleted 无对应回调：静默忽略，不报错
  (check log => '((ctx-token "modified" "/a/b")))
) ;let*

;; 4. handler → 源码串列表：仅序列化非 #f 槽，供 worker 会话重建
(let ((h (make-fs-event-handler :on-deleted (lambda (ctx ev) 'del))))
  (let ((src-list (fs-watch-handler->src-list h)))
    (check (length src-list) => 1)
    (check (car (car src-list)) => "deleted")
    (check-true (string-contains? (cdr (car src-list)) "lambda"))
  ) ;let
) ;let

;; 4c. worker 会话重建诊断：go-result 在 worker 里执行 make-dispatcher 并回传结果
(let* ((rch (go-result
              (let* ((src-list
                      '(("modified" . "(lambda (ctx ev) (list 'rebuilt ctx (fs-event-path ev)))")))
                     (cbs (fs-watch-make-dispatcher src-list))
                    ) ;
                (if (procedure? (cdr (assoc "modified" cbs)))
                  (list 'ok ((cdr (assoc "modified" cbs)) 'tok
                        (%fs-event-from-alist '((type . "modified") (path . "/w/h")))
                    ) ;call
                  ) ;if
                  (list 'bad cbs)
                ) ;if
              ) ;let*
            ) ;go-result
       ) ;rch
       (r (go-result-recv! rch 10000))
      ) ;
  (check r => '(ok (rebuilt tok "/w/h")))
) ;let*

;; 4d. args 路径最小复现：cbs-src 经 go-result vars 序列化传入 worker
(let* ((src '(("modified" . "(lambda (ctx ev) 'ok)")))
       (rch (go-result (src)
               (let ((cbs (fs-watch-pump-diag src)))
                 (procedure? (cdr (assoc "modified" cbs)))
               ) ;let
             ) ;go-result
       ) ;rch
       (r (go-result-recv! rch 10000))
      ) ;
  (check r => #t)
) ;let*

;; 4b. 主会话直接重建：回调闭包变量在设计上不可访问（worker 语义），
;;     仅验证重建产物可调用且不抛错
(let* ((h (make-fs-event-handler
            :on-modified (lambda (ctx ev) (list ctx (fs-event-path ev)))
          ) ;make-fs-event-handler
       ) ;h
       (cbs (fs-watch-make-dispatcher (fs-watch-handler->src-list h)))
      ) ;
  (check-true (procedure? (cdr (assoc "modified" cbs))))
  (fs-watch-dispatch! cbs 'tok (%make-fs-event-for-test "modified" "/p/h" #f))
) ;let*

;; ============================================================
;; 三、C 层直连（g_fs_watch_start/poll/stop，主会话直接调用）
;; ============================================================

;; 5. 创建/修改/删除文件产生对应事件；stop 后 poll 返回 'stopped
(let ((handle (fs-watch-start% %test-dir #f)))
  (check-true (integer? handle))
  ;; created：新建探测文件（macOS 上表现为目录级 modified）
  (%probe-write "probe-a.txt" "v1")
  (let ((res (%poll-until
               (lambda (events)
                 (any (lambda (e)
                        (and (pair? (assoc 'type e))
                             (string=? (cdr (assoc 'type e)) (%expect-type "created"))
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
    (unless (cdr res)
      (display ";;; diagnostic events: ")
      (write %event-log)
      (newline)
    ) ;unless
    (check-true (cdr res))
  ) ;let
  (set! %event-log '())
  ;; modified：改写同一文件
  (%probe-write "probe-a.txt" "v2")
  (let ((res (%poll-until
               (lambda (events)
                 (any (lambda (e)
                        (and (pair? (assoc 'type e))
                             (string=? (cdr (assoc 'type e)) (%expect-type "modified"))
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
  (let ((res (%poll-until
               (lambda (events)
                 (any (lambda (e)
                        (and (pair? (assoc 'type e))
                             (string=? (cdr (assoc 'type e)) (%expect-type "deleted"))
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


;; 6. 参数校验（Scheme 层前置校验，C 层哨兵 #f）
(check-catch 'type-error (fs-watch-start% 123 #f))
(check-catch 'type-error (fs-watch-start% %test-dir "yes"))
(check-catch 'value-error (fs-watch-start% %test-dir #t))
(check-catch 'value-error (fs-watch-start% (string-append %test-dir "/no-such-dir") #f))

;; 7. C 层多轮启停稳定性：连续 5 轮 start/poll/stop 不崩溃不挂起
(let loop ((n 5))
  (when (> n 0)
    (let ((h (fs-watch-start% %test-dir #f)))
      (check-true (integer? h))
      (check (g_fs_watch_poll h 50) => #f)
      (check (g_fs_watch_stop h) => #t)
    ) ;let
    (loop (- n 1))
  ) ;when
) ;let

(rmdir %test-dir)

(check-report)
