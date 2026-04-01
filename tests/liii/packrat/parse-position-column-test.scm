(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-position-column
;; 获取解析位置的列号。
;;
;; 语法
;; ----
;; (parse-position-column position)
;;
;; 参数
;; ----
;; position : parse-position
;;   parse-position 对象
;;
;; 返回值
;; ----
;; number
;;   列号（从 0 开始）
;;
;; 描述
;; ----
;; 提取位置信息中的列号部分，列号从 0 开始计数。

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-column pos) => 15)
) ;let

(let ()
  (define pos-start (make-parse-position "file.scm" 1 0))
  (check (parse-position-column pos-start) => 0)
) ;let

(let ()
  (define pos-large (make-parse-position "big.scm" 100 999))
  (check (parse-position-column pos-large) => 999)
) ;let

(let ()
  (define pos-no-file (make-parse-position #f 10 5))
  (check (parse-position-column pos-no-file) => 5)
) ;let

(check-report)
