;; fs-watch-make-dispatcher
;; 在调用方环境重建回调表（worker 会话 pump 的配套入口）。
;;
;; 语法
;; ----
;; (fs-watch-make-dispatcher src-list)
;;
;; 参数
;; ----
;; src-list : list
;; 形如 (('modified . "(lambda (ctx ev) ...)") ...) 的源码字符串列表。
;;
;; 返回值
;; ----
;; list
;; ((type . procedure) ...)，供 fs-watch-dispatch! 分发。

(import (scheme base) (liii check) (liii string) (liii go) (liii fs-watch))

(check-set-mode! 'report-failed)

;; 主会话直接重建：回调闭包变量在设计上不可访问（worker 语义），
;; 仅验证重建产物可调用且不抛错
(let* ((h
         (make-fs-event-handler
           :on-modified (lambda (ctx ev) (list ctx (fs-event-path ev)))
         ) ;make-fs-event-handler
       ) ;h
       (cbs (fs-watch-make-dispatcher (fs-watch-handler->src-list h)))
      ) ;
  (check-true (procedure? (cdr (assoc 'modified cbs))))
  (fs-watch-dispatch! cbs 'tok (make-fs-event 'modified "/p/h" #f))
) ;let*

;; worker 会话内重建并调用（go-result 全链路）
(let* ((rch
         (go-result
           (let* ((src-list '((modified
                                . "(lambda (ctx ev) (list 'rebuilt ctx (fs-event-path ev)))"))
                  ) ;src-list
                  (cbs (fs-watch-make-dispatcher src-list))
                 ) ;
             (if (procedure? (cdr (assoc 'modified cbs)))
               (list 'ok
                ((cdr (assoc 'modified cbs))
                 'tok
                 (%fs-event-from-alist '((type . modified) (path . "/w/h")))
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
