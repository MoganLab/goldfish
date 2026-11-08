;; stream-iterate
;; 迭代应用过程生成 base (proc base) (proc (proc base)) ... 的流。
;;
;; 语法
;; ----
;; (stream-iterate proc base)
;;
;; 参数
;; ----
;; proc : procedure?
;; 迭代函数。
;;
;; base : any
;; 初始值。
;; 返回值
;; ----
;; stream?
;; 迭代序列无限流，首元素为 base。
;;
;; 说明
;; ----
;; 错误：proc 非过程报错。

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'stream-iterate (stream-iterate "four" 0))
(check (stream->list 3 (stream-iterate (lambda (x) (+ x 1)) 1)) => '(1 2 3))
(check (stream->list 4 (stream-iterate (lambda (x) (* 2 x)) 1)) => '(1 2 4 8))

(check-report)
