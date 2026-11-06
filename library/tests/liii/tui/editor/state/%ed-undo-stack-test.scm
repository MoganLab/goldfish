;; %ed-undo-stack
;; 获取编辑器内部数据记录的 undo-stack 字段值。
;;
;; 语法
;; ----
;; (%ed-undo-stack data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; undo-stack 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-undo-stack-set! d '(1))
  (check (%ed-undo-stack d) => '(1))
) ;let

(check-report)
