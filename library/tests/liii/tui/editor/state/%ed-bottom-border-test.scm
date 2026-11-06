;; %ed-bottom-border
;; 获取编辑器内部数据记录的 bottom-border 字段值。
;;
;; 语法
;; ----
;; (%ed-bottom-border data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; bottom-border 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-bottom-border-set! d #t)
  (check (%ed-bottom-border d) => #t)
) ;let

(check-report)
