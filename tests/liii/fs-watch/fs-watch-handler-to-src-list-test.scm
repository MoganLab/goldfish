(import (scheme base) (liii check) (liii string) (liii go) (liii fs-watch))

(check-set-mode! 'report-failed)

;; fs-watch-handler->src-list
;; 将事件处理器的回调槽序列化为源码字符串列表，供 go worker 会话重建。
;;
;; 语法
;; ----
;; (fs-watch-handler->src-list handler) → list?
;;
;; 参数
;; ----
;; handler : fs-event-handler?
;; 事件处理器。
;;
;; 返回值
;; -----
;; list?
;; 形如 (("modified" . "(lambda (ctx ev) ...)") ...)，仅含非 #f 槽。
;;
;; 说明
;; ----
;; 1. 过程无法跨 (liii go) 会话序列化，改为源码字符串传递，
;;    worker 侧由 fs-watch-make-dispatcher eval 重建。
;; 2. 因此回调体内的自由符号必须是库导出绑定（跨线程状态经
;;    :context 显式传递），不能闭包捕获主会话对象。

(let ((h (make-fs-event-handler :on-deleted (lambda (ctx ev) 'del))))
  (let ((src-list (fs-watch-handler->src-list h)))
    (check (length src-list) => 1)
    (check (car (car src-list)) => "deleted")
    (check-true (string-contains? (cdr (car src-list)) "lambda"))
  ) ;let
) ;let

;; 全部槽为 #f 时返回空表
(check (fs-watch-handler->src-list (make-fs-event-handler)) => '())

;; fs-watch-make-dispatcher
;; 在调用方环境重建回调表（worker 会话 pump 的配套入口）。
;;
;; 语法
;; ----
;; (fs-watch-make-dispatcher src-list) → list?
;;
;; 返回 ((type . procedure) ...)，供 fs-watch-dispatch! 分发。

;; 主会话直接重建：回调闭包变量在设计上不可访问（worker 语义），
;; 仅验证重建产物可调用且不抛错
(let* ((h
         (make-fs-event-handler
           :on-modified (lambda (ctx ev) (list ctx (fs-event-path ev)))
         ) ;make-fs-event-handler
       ) ;h
       (cbs (fs-watch-make-dispatcher (fs-watch-handler->src-list h)))
      ) ;
  (check-true (procedure? (cdr (assoc "modified" cbs))))
  (fs-watch-dispatch! cbs 'tok (make-fs-event "modified" "/p/h" #f))
) ;let*

;; worker 会话内重建并调用（go-result 全链路）
(let* ((rch
         (go-result
           (let* ((src-list '(("modified"
                               . "(lambda (ctx ev) (list 'rebuilt ctx (fs-event-path ev)))"))
                  ) ;src-list
                  (cbs (fs-watch-make-dispatcher src-list))
                 ) ;
             (if (procedure? (cdr (assoc "modified" cbs)))
               (list 'ok
                ((cdr (assoc "modified" cbs))
                 'tok
                 (%fs-event-from-alist '((type . "modified") (path . "/w/h")))
                ) ;
               ) ;list
               (list 'bad cbs)
             ) ;if
           ) ;let*
         ) ;go-result
       ) ;rch
       (r (go-result-recv! rch 10000))
      ) ;
  (check r => '(ok (rebuilt tok "/w/h")))
) ;let*

(check-report)
