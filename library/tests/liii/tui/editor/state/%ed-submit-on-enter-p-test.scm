;; %ed-submit-on-enter?
;; 获取编辑器内部数据记录的 submit-on-enter? 字段值。
;;
;; 语法
;; ----
;; (%ed-submit-on-enter? data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; submit-on-enter? 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-submit-on-enter?-set! d #t)
  (check (%ed-submit-on-enter? d) => #t)
) ;let

(check-report)
