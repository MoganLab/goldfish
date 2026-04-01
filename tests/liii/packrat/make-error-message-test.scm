(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; make-error-message
;; 构造带有错误消息的解析错误对象。
;;
;; 语法
;; ----
;; (make-error-message position message)
;;
;; 参数
;; ----
;; position : parse-position
;;   解析位置
;; message : string
;;   错误消息字符串
;;
;; 返回值
;; ----
;; parse-error
;;   表示带有错误消息的解析错误对象
;;
;; 描述
;; ----
;; 当需要返回自定义错误消息时构造错误对象，
;; 提供更详细的错误描述信息。

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-msg (make-error-message pos "syntax error"))
  (check-true (parse-error? error-msg))
) ;let

(let ()
  (define pos (make-parse-position "main.gf" 42 5))
  (define error2 (make-error-message pos "unexpected end of input"))
  (check-true (parse-error? error2))
) ;let

(let ()
  (define pos (make-parse-position "calc.scm" 10 15))
  (define error3 (make-error-message pos "division by zero"))
  (check-true (parse-error? error3))
) ;let

(check-report)
