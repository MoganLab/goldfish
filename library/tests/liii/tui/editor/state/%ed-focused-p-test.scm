;; %ed-focused?
;; 获取编辑器内部数据记录的 focused? 字段值。
;;
;; 语法
;; ----
;; (%ed-focused? data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; focused? 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-focused?-set! d #t)
  (check (%ed-focused? d) => #t)
) ;let

(check-report)
