;; %ed-top-row
;; 获取编辑器内部数据记录的 top-row 字段值。
;;
;; 语法
;; ----
;; (%ed-top-row data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; top-row 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-top-row-set! d 0)
  (check (%ed-top-row d) => 0)
) ;let

(check-report)
