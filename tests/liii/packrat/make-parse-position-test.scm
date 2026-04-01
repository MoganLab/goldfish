(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; make-parse-position
;; 构造解析位置对象。
;;
;; 语法
;; ----
;; (make-parse-position filename line column)
;;
;; 参数
;; ----
;; filename : string or #f
;;   文件名，#f 表示无文件名
;; line : number
;;   行号（从 1 开始）
;; column : number
;;   列号（从 0 开始）
;;
;; 返回值
;; ----
;; parse-position
;;   表示文件中的位置信息
;;
;; 描述
;; ----
;; 创建表示源代码位置的记录，用于错误报告和调试。
;; 行号从 1 开始，列号从 0 开始。

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check-true (parse-position? pos))
  (check (parse-position-file pos) => "test.scm")
  (check (parse-position-line pos) => 3)
  (check (parse-position-column pos) => 15)
) ;let

(let ()
  (define pos-no-file (make-parse-position #f 1 0))
  (check-true (parse-position? pos-no-file))
  (check (parse-position-file pos-no-file) => #f)
  (check (parse-position-line pos-no-file) => 1)
  (check (parse-position-column pos-no-file) => 0)
) ;let

(let ()
  (define pos-start (make-parse-position "main.scm" 1 0))
  (check (parse-position-line pos-start) => 1)
  (check (parse-position-column pos-start) => 0)
) ;let

(check-report)
