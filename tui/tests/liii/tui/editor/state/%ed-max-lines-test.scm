;; %ed-max-lines
;; 获取编辑器内部数据记录的 max-lines 字段值。
;;
;; 语法
;; ----
;; (%ed-max-lines data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; max-lines 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-max-lines-set! d 10)
  (check (%ed-max-lines d) => 10)
) ;let

(check-report)
