;; stream-let
;; 流的命名循环绑定，类似命名 let 但循环体惰性。
;;
;; 语法
;; ----
;; (stream-let tag ((name val) ...) body ...)
;;
;; 参数
;; ----
;; tag : 标识符
;; 循环名，body 中以 (tag val ...) 递归。
;;
;; (name val) : 绑定对
;;
;; body : expression ...
;; 返回值
;; ----
;; stream?
;; 循环体求值产物流。
;;
;; 说明
;; ----
;; SRFI 41：递归调用出现在尾位置时不占栈，是表达迭代惰性算法的关键形式。

;;
;; 错误处理
;; ----
;; 无

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check
  (stream->list
    (stream-let loop
     ((strm (stream 1 2 3)))
     (if (stream-null? strm)
       stream-null
       (stream-cons (* 2 (stream-car strm)) (loop (stream-cdr strm)))
     ) ;if
    ) ;stream-let
  ) ;stream->list
  =>
  '(2 4 6)
) ;check

(define evens (stream-let loop ((n 0)) (stream-cons n (loop (+ n 2)))))
(check (stream->list 3 evens) => '(0 2 4))

(check-report)
