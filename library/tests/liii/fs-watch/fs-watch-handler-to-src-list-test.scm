;; fs-watch-handler->src-list
;; 将事件处理器的回调槽序列化为源码字符串列表，供 go worker 会话重建。
;;
;; 语法
;; ----
;; (fs-watch-handler->src-list handler)
;;
;; 参数
;; ----
;; handler : fs-event-handler?
;; 事件处理器。
;;
;; 返回值
;; ----
;; list
;; 形如 (('modified . "(lambda (ctx ev) ...)") ...)，仅含非 #f 槽。
;;
;; 说明
;; ----
;; 1. 过程无法跨 (liii go) 会话序列化，改为源码字符串传递，
;;    worker 侧由 fs-watch-make-dispatcher eval 重建。
;; 2. 因此回调体内的自由符号必须是库导出绑定（跨线程状态经
;;    :context 显式传递），不能闭包捕获主会话对象。

(import (scheme base) (liii check) (liii string) (liii fs-watch))

(check-set-mode! 'report-failed)

(let ((h (make-fs-event-handler :on-deleted (lambda (ctx ev) 'del))))
  (let ((src-list (fs-watch-handler->src-list h)))
    (check (length src-list) => 1)
    (check (car (car src-list)) => 'deleted)
    (check-true (string-contains? (cdr (car src-list)) "lambda"))
  ) ;let
) ;let

;; 全部槽为 #f 时返回空表
(check (fs-watch-handler->src-list (make-fs-event-handler)) => '())

(check-report)
