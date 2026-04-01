(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-position-line
;; 获取解析位置的行号。
;;
;; 语法
;; ----
;; (parse-position-line position)
;;
;; 参数
;; ----
;; position : parse-position
;;   parse-position 对象
;;
;; 返回值
;; ----
;; number
;;   行号（从 1 开始）
;;
;; 描述
;; ----
;; 提取位置信息中的行号部分，行号从 1 开始计数。

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-line pos) => 3)
) ;let

(let ()
  (define pos-first (make-parse-position "file.scm" 1 0))
  (check (parse-position-line pos-first) => 1)
) ;let

(let ()
  (define pos-large (make-parse-position "big.scm" 1000 50))
  (check (parse-position-line pos-large) => 1000)
) ;let

(let ()
  (define pos-no-file (make-parse-position #f 42 8))
  (check (parse-position-line pos-no-file) => 42)
) ;let

(check-report)
