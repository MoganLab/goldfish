;; %ed-placeholder
;; 获取编辑器内部数据记录的 placeholder 字段值。
;;
;; 语法
;; ----
;; (%ed-placeholder data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; placeholder 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-placeholder-set! d "ph1")
  (check (%ed-placeholder d) => "ph1")
) ;let

(check-report)
