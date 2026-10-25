;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; 文件系统监听（API 对齐 Python watchdog）：
;;   (make-fs-event-handler :on-modified (lambda (ctx ev) ...) ...)
;;   (observer-schedule! obs handler dir :recursive #f :context ctx)
;;   (observer-start! obs) / (observer-stop! obs)
;;
;; 线程语义与 watchdog 一致：回调在后台 (liii go) worker 会话中执行，
;; 不在主会话。跨线程状态请经 :context 显式传递可序列化值（典型为 channel，
;; 等价 Python 回调持有 queue.Queue 的模式）；回调体内引用的自由符号必须是
;; 库导出绑定（如 chan-send! / fs-event-path），不能闭包捕获主会话对象。
(define-library (liii fs-watch)
  (export make-fs-event-handler fs-event-handler? fs-handler-on-modified
    fs-handler-on-created fs-handler-on-deleted fs-handler-on-moved
    make-fs-event fs-event? fs-event-type fs-event-path fs-event-dest-path
    make-observer observer? observer-schedule! observer-start! observer-stop!
    fs-watch-dispatch! fs-watch-make-dispatcher fs-watch-handler->src-list
    fs-watch-pump fs-watch-start%
    ;; 仅限 (liii go) worker 会话内使用的实现细节（导出因子必须可见）
    %fs-event-from-alist
  ) ;export
  (import (scheme base)
    (scheme eval)
    (scheme read)
    (liii base)
    (liii string)
    (liii list)
    (liii error)
  ) ;import
  (begin

    ;; ------------------------------------------------------------
    ;; 事件与 handler record
    ;; ------------------------------------------------------------

    (define-record-type <fs-event>
      (%make-fs-event type path dest)
      fs-event?
      (type fs-event-type)
      (path fs-event-path)
      (dest fs-event-dest-path)
    ) ;define-record-type

    ;; 公开事件构造器（对齐 watchdog 的 FileSystemEvent 可实例化语义）：
    ;; (make-fs-event "modified" "/path" [:dest #f])
    (define* (make-fs-event type path (dest #f))
      (unless (string? type)
        (type-error "make-fs-event: type must be a string" type)
      ) ;unless
      (unless (string? path)
        (type-error "make-fs-event: path must be a string" path)
      ) ;unless
      (unless (or (not dest) (string? dest))
        (type-error "make-fs-event: dest must be a string or #f" dest)
      ) ;unless
      (%make-fs-event type path dest)
    ) ;define*

    (define-record-type <fs-event-handler>
      (%make-fs-event-handler on-modified on-created on-deleted on-moved)
      fs-event-handler?
      (on-modified fs-handler-on-modified)
      (on-created fs-handler-on-created)
      (on-deleted fs-handler-on-deleted)
      (on-moved fs-handler-on-moved)
    ) ;define-record-type

    (define* (make-fs-event-handler (on-modified #f)
               (on-created #f)
               (on-deleted #f)
               (on-moved #f)
             ) ;make-fs-event-handler

      (for-each
        (lambda (cb)
          (unless (or (not cb) (procedure? cb))
            (type-error "make-fs-event-handler: callbacks must be procedures or #f")
          ) ;unless
        ) ;lambda
        (list on-modified on-created on-deleted on-moved)
      ) ;for-each
      (%make-fs-event-handler on-modified on-created on-deleted on-moved)
    ) ;define*

    ;; ------------------------------------------------------------
    ;; 分发核心（纯逻辑，主会话单测与 worker 会话共用）
    ;; ------------------------------------------------------------

    ;; handler → (("modified" . "(lambda (ctx ev) ...)") ...)：
    ;; 过程不可跨 go 会话序列化，改为源码字符串传递，worker 侧 eval 重建
    (define (fs-watch-handler->src-list handler)
      (filter pair?
        (map
          (lambda (slot)
            (let ((cb (cdr slot)))
              (if cb (cons (car slot) (object->string (procedure-source cb))) #f)
            ) ;let
          ) ;lambda
          (list (cons "modified" (fs-handler-on-modified handler))
            (cons "created" (fs-handler-on-created handler))
            (cons "deleted" (fs-handler-on-deleted handler))
            (cons "moved" (fs-handler-on-moved handler))
          ) ;list
        ) ;map
      ) ;filter
    ) ;define

    ;; 源码串列表 → ((type . procedure) ...)；
    ;; 使用 s7 内置 eval-string 直接求值（默认当前环境）
    (define (fs-watch-make-dispatcher src-list)
      (map
        (lambda (p) (cons (car p) (eval-string (cdr p))))
        src-list
      ) ;map
    ) ;define

    ;; 按事件类型调用对应回调 (ctx event)；无对应槽则静默忽略；
    ;; 回调异常被捕获并吞掉，避免单次错误终止整个监听循环（对齐 watchdog 线程韧性）
    (define (fs-watch-dispatch! cbs ctx event)
      (let ((entry (assoc (fs-event-type event) cbs)))
        (when entry
          (catch #t (lambda () ((cdr entry) ctx event)) (lambda args #f))
        ) ;when
      ) ;let
    ) ;define

    ;; C 层事件 alist（key 为符号 type/path/dest）→ <fs-event> record
    ;; （导出：fs-watch-pump 在 go worker 会话以源码执行，自由符号必须是导出绑定）
    (define (%fs-event-from-alist e)
      (let ((type (cdr (assoc 'type e)))
            (path (cdr (assoc 'path e)))
            (dest-pair (assoc 'dest e))
           ) ;
        (%make-fs-event type path (if dest-pair (cdr dest-pair) #f))
      ) ;let
    ) ;define

    ;; ------------------------------------------------------------
    ;; worker 会话侧的监听泵（必须顶层导出：go 以 procedure-source 执行）
    ;; ------------------------------------------------------------

    (define (fs-watch-pump handle cbs-src ctx)
      (let ((cbs (fs-watch-make-dispatcher cbs-src)))
        (let loop
          ()
          (let ((evs (g_fs_watch_poll handle 1000)))
            (cond ((eq? evs 'stopped) 'stopped)
                  ((pair? evs)
                   (for-each (lambda (ev) (fs-watch-dispatch! cbs ctx (%fs-event-from-alist ev)))
                     evs
                   ) ;for-each
                   (loop)
                  ) ;
                  (else (loop))
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define

    ;; 带前置校验的 start 包装：C 层错误统一返回 #f 哨兵，
    ;; 错误在此拋出（避免 C++ 内 s7_error 跨线程活跃过的栈帧 longjmp）
    (define (fs-watch-start% path recursive)
      (cond ((not (string? path)) (type-error "fs-watch: path must be a string" path))
            ((not (boolean? recursive))
             (type-error "fs-watch: recursive must be a boolean" recursive)
            ) ;
            (recursive (value-error "fs-watch: recursive watching is not supported yet"))
            (else
              (let ((handle (g_fs_watch_start path recursive)))
                (if (integer? handle)
                  handle
                  (value-error "fs-watch: cannot start watching (path missing or unsupported)"
                    path
                  ) ;value-error
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ------------------------------------------------------------
    ;; Observer（对齐 watchdog Observer.schedule/start/stop）
    ;; ------------------------------------------------------------

    (define-record-type <fs-observer>
      (%make-fs-observer schedules handles running?)
      observer?
      (schedules %fs-observer-schedules %fs-observer-schedules-set!)
      (handles %fs-observer-handles %fs-observer-handles-set!)
      (running? %fs-observer-running? %fs-observer-running?-set!)
    ) ;define-record-type

    (define (make-observer)
      (%make-fs-observer '() '() #f)
    ) ;define

    (define (%kw-value args key default)
      (let loop
        ((rest args))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 注册监听项：start 前可多次累积；running 中注册会抛 value-error（对齐
    ;; watchdog 的 schedule 语义差异见文档：v1 要求 start 前完成注册）
    (define (observer-schedule! obs handler path . opts)
      (unless (observer? obs)
        (type-error "observer-schedule!: obs must be an observer")
      ) ;unless
      (unless (fs-event-handler? handler)
        (type-error "observer-schedule!: handler must be an fs-event-handler")
      ) ;unless
      (unless (string? path)
        (type-error "observer-schedule!: path must be a string")
      ) ;unless
      (when (%fs-observer-running? obs)
        (value-error "observer-schedule!: observer is running, stop it first")
      ) ;when
      (let ((recursive (%kw-value opts :recursive #f)) (ctx (%kw-value opts :context #f)))
        (%fs-observer-schedules-set! obs
          (append (%fs-observer-schedules obs) (list (list handler path recursive ctx)))
        ) ;%fs-observer-schedules-set!
      ) ;let
    ) ;define

    ;; 启动：每项创建 C 层 watcher 并派发后台 go 泵线程；
    ;; 回调源码列表与 context 均为可序列化值，可安全跨越 go 会话边界。
    ;; 不走通用 go 宏（其 prelude 会 import 主会话全部活跃库，若含
    ;; tools/pi 等非主库路径的库，worker 会话将加载失败），
    ;; 此处手工构造任务：prelude 精确限定为主库的 (liii fs-watch)/(liii go)。
    (define (observer-start! obs)
      (unless (observer? obs)
        (type-error "observer-start!: obs must be an observer")
      ) ;unless
      (when (%fs-observer-running? obs)
        (value-error "observer-start!: observer is already running")
      ) ;when
      (for-each
        (lambda (sched)
          (let* ((handler (car sched))
                 (path (cadr sched))
                 (recursive (caddr sched))
                 (ctx (cadddr sched))
                 (handle (fs-watch-start% path recursive))
                 (cbs-src (fs-watch-handler->src-list handler))
                 (pump-src (procedure-source fs-watch-pump))
                ) ;
            (%fs-observer-handles-set! obs (cons handle (%fs-observer-handles obs)))
            (g_go-spawn (list 'handle 'cbs-src 'ctx)
              (list handle cbs-src ctx)
              (list 'begin
                '(import (liii fs-watch) (liii go))
                (list pump-src 'handle 'cbs-src 'ctx)
              ) ;list
            ) ;g_go-spawn
          ) ;let*
        ) ;lambda
        (%fs-observer-schedules obs)
      ) ;for-each
      (%fs-observer-running?-set! obs #t)
    ) ;define

    ;; 停止：逐个停掉 C 层 watcher（泵线程收到 'stopped 后自行退出）；
    ;; 幂等，可重复调用；stop 后允许重新 schedule + start
    (define (observer-stop! obs)
      (unless (observer? obs)
        (type-error "observer-stop!: obs must be an observer")
      ) ;unless
      (for-each g_fs_watch_stop (%fs-observer-handles obs))
      (%fs-observer-handles-set! obs '())
      (%fs-observer-running?-set! obs #f)
    ) ;define

  ) ;begin
) ;define-library
