(import (liii check)
        (liii packrat)
) ;import

(check-set-mode! 'report-failed)

;; parse-position-file
;; 获取解析位置关联的文件名。
;;
;; 语法
;; ----
;; (parse-position-file position)
;;
;; 参数
;; ----
;; position : parse-position
;;   parse-position 对象
;;
;; 返回值
;; ----
;; string or #f
;;   关联的文件名，或 #f 表示文件名未知
;;
;; 描述
;; ----
;; 提取位置信息中的文件名部分。

(let ()
  (define pos (make-parse-position "test.scm" 3 15))
  (check (parse-position-file pos) => "test.scm")
) ;let

(let ()
  (define pos (make-parse-position "path/to/file.gf" 10 5))
  (check (parse-position-file pos) => "path/to/file.gf")
) ;let

(let ()
  (define pos-no-file (make-parse-position #f 1 0))
  (check (parse-position-file pos-no-file) => #f)
) ;let

(let ()
  (define pos-empty (make-parse-position "" 1 0))
  (check (parse-position-file pos-empty) => "")
) ;let

(check-report)
