(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-error-position
;; 获取解析错误的位置信息。
;;
;; 语法
;; ----
;; (parse-error-position error)
;;
;; 参数
;; ----
;; error : parse-error
;;   parse-error 对象
;;
;; 返回值
;; ----
;; parse-position or #f
;;   解析位置的 parse-position 对象，或 #f 表示未知位置
;;
;; 描述
;; ----
;; 从解析错误中提取位置信息，用于错误报告。

(let ()
  (define pos (make-parse-position "test.scm" 2 10))
  (define error-ex (make-error-expected pos "open-paren"))
  (check (parse-error-position error-ex) => pos)
) ;let

(let ()
  (define pos (make-parse-position "main.gf" 42 5))
  (define error-msg (make-error-message pos "syntax error"))
  (check (parse-error-position error-msg) => pos)
) ;let

(let ()
  (define pos (make-parse-position #f 1 0))
  (define error3 (make-error-expected pos "num"))
  (check (parse-error-position error3) => pos)
) ;let

(let ()
  (define pos (make-parse-position "big.scm" 1000 80))
  (define error4 (make-error-message pos "unexpected token"))
  (check (parse-position-line (parse-error-position error4)) => 1000)
  (check (parse-position-column (parse-error-position error4)) => 80)
) ;let

(check-report)
