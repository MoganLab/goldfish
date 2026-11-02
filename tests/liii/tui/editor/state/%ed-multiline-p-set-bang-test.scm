;; %ed-multiline?-set!
;; 设置编辑器内部数据记录的 multiline? 字段值。
;;
;; 语法
;; ----
;; (%ed-multiline?-set! data val)
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
  (%ed-multiline?-set! d #f)
  (check (%ed-multiline? d) => #f)
) ;let

(check-report)
