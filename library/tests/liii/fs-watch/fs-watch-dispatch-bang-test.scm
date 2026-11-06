;; fs-watch-dispatch!
;; 按事件类型调用对应回调表中的回调（分发核心，主会话单测与 worker 泵共用）。
;;
;; 语法
;; ----
;; (fs-watch-dispatch! cbs ctx event)
;;
;; 参数
;; ----
;; cbs : list
;; 回调表，形如 ((modified . procedure) ...)，通常由
;; fs-watch-make-dispatcher 重建得到。
;; ctx : any
;; 传给回调的上下文（原样透传）。
;; event : fs-event?
;; 事件记录。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 1. 按事件的 type 在 cbs 中查找对应回调并调用 (cb ctx event)。
;; 2. 无对应回调槽时静默忽略，不报错。
;; 3. 回调抛出的异常被捕获并吞掉，避免单次错误终止整个监听循环
;;    （对齐 watchdog observer 线程的韧性语义）。

(import (scheme base) (liii check) (liii fs-watch))

(check-set-mode! 'report-failed)

(let* ((log '())
       (cbs
         (list
           (cons 'modified
             (lambda (ctx ev)
               (set! log (cons (list ctx (fs-event-type ev) (fs-event-path ev)) log))
             ) ;lambda
           ) ;cons
         ) ;list
       ) ;cbs
      ) ;
  (fs-watch-dispatch! cbs 'ctx-token (make-fs-event 'modified "/a/b" #f))
  (fs-watch-dispatch! cbs 'ctx-token (make-fs-event 'deleted "/a/b" #f))
  (check (length log) => 1)
  (check (car log) => '(ctx-token modified "/a/b"))
) ;let*

;; 无对应回调槽时静默忽略
(fs-watch-dispatch! '() 'ctx-token (make-fs-event 'modified "/a/b" #f))

;; 回调抛出的异常被吞掉
(let ((cbs (list (cons 'modified (lambda (ctx ev) (error 'boom "callback failed"))))))
  (fs-watch-dispatch! cbs 'ctx-token (make-fs-event 'modified "/a/b" #f))
) ;let

(check-report)
