;; %ed-top-border-set!
;; 设置编辑器内部数据记录的 top-border 字段值。
;;
;; 语法
;; ----
;; (%ed-top-border-set! data val)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; val : any
;; 待设置的新值。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-top-border-set! d #f)
  (check (%ed-top-border d) => #f)
) ;let

(check-report)
