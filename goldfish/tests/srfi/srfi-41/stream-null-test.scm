;; stream-null
;; 空流常量。
;;
;; 语法
;; ----
;; stream-null
;;
;; 参数
;; ----
;; 无（常量）
;;
;; 返回值
;; ----
;; stream?
;; 空流对象；stream-null? 为 #t、stream-pair? 为 #f。
;;
;; 说明
;; ----
;; SRFI 41：全部空流的 force 结果共享同一记忆化内容。
;;
;; 错误处理
;; ----
;; 无

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check (stream? stream-null) => #t)
(check (stream-null? stream-null) => #t)
(check (stream-pair? stream-null) => #f)
(check (stream->list stream-null) => '())

(check-report)
