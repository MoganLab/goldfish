;; stream-cons
;; 流版本的 cons：构造头元素与尾流，两者均延迟求值。
;;
;; 语法
;; ----
;; (stream-cons obj strm)
;;
;; 参数
;; ----
;; obj : any
;; 头元素，延迟求值。
;; strm : stream?
;; 尾流，延迟求值。
;;
;; 返回值
;; ----
;; stream?
;; 流对；stream-pair? 为 #t。
;;
;; 说明
;; ----
;; SRFI 41：obj 与 strm 仅在 stream-car/stream-cdr 被调用时求值。
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check (stream? (stream-cons 1 stream-null)) => #t)
(check (stream-null? (stream-cons 1 stream-null)) => #f)
(check (stream-pair? (stream-cons 1 stream-null)) => #t)
(check (stream-car (stream-cons 1 stream-null)) => 1)

(check-report)
