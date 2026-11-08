;; stream-match
;; 按模式子句匹配流，类似 list 的 match。
;;
;; 语法
;; ----
;; (stream-match strm-expr (pattern [fender] expr) ...)
;;
;; 参数
;; ----
;; strm-expr : 产生流的表达式
;;
;; pattern : 模式
;; _ 通配、() 空流、(p . p) 流对、变量 捕获整流。
;;
;; fender : 保护条件（可选）
;;
;; expr : 匹配成功时的结果表达式
;; 返回值
;; ----
;; any
;; 第一个模式（与 fender）匹配成功的子句的 expr 值。
;;
;; 说明
;; ----

;;
;; 错误处理
;; ----
;; type-error
;; strm-expr 结果不是流
;; value-error
;; 全部模式子句匹配失败

(import (liii check) (srfi srfi-41))

(check-set-mode! 'report-failed)

(check-catch 'type-error (stream-match '(1 2 3) (_ 'ok)))
(check-catch 'value-error (stream-match (stream 1 2 3) (() 42)))
(check (stream-match stream-null (() 'ok)) => 'ok)
(check (stream-match (stream 1) (() 'no) ((a) a)) => 1)
(check (stream-match (stream 1) (() 'no) ((_) 'ok)) => 'ok)
(check (stream-match (stream 1 2 3) ((a b c) (list a b c))) => '(1 2 3))
(check (stream-match (stream 1 2 3) ((a . _) a)) => 1)
(check
  (stream-match (stream 1 2 3) ((a b . c) (list a b (stream-car c))))
  =>
  '(1 2 3)
) ;check
(check (stream-match (stream 1 2 3) (s (stream->list s))) => '(1 2 3))
(check (stream-match (stream 1 2 3) ((a . _) (= a 1) 'ok)) => 'ok)
(check (stream-match (stream 1 2 3) ((a . _) (= a 2) 'yes) (_ 'no)) => 'no)

(check-report)
