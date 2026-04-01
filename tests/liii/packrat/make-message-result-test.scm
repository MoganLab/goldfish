(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; make-message-result
;; 构造表示带有错误消息的 parse-result 对象。
;;
;; 语法
;; ----
;; (make-message-result position message)
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
;; parse-result
;;   表示带有错误消息的失败解析结果
;;
;; 描述
;; ----
;; 当需要返回自定义错误消息时使用此函数，
;; 比 make-expected-result 提供更详细的错误信息。

(let ()
  (define pos (make-parse-position "test.scm" 1 5))
  (define message (make-message-result pos "syntax error"))
  (check-false (parse-result-successful? message))
  (check-true (parse-result? message))
) ;let

(let ()
  (define pos (make-parse-position "calc.scm" 10 3))
  (define msg2 (make-message-result pos "unexpected end of input"))
  (check-false (parse-result-successful? msg2))
) ;let

(let ()
  (define pos (make-parse-position "main.scm" 42 8))
  (define msg3 (make-message-result pos "invalid character '\x00'"))
  (check-true (parse-result? msg3))
) ;let

(check-report)
