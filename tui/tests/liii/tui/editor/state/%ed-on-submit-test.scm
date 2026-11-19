;; %ed-on-submit
;; 获取编辑器内部数据记录的 on-submit 字段值。
;;
;; 语法
;; ----
;; (%ed-on-submit data)
;;
;; 参数
;; ----
;; data : %editor-data?
;; 编辑器内部数据记录。
;;
;; 返回值
;; ----
;; any
;; on-submit 字段值。

(import (scheme base)
  (liii check)
  (liii tui editor state)
) ;import

(check-set-mode! 'report-failed)

(let ((d (%make-editor-data #f #f #f #t "" 'block #f #f #f #f #f #t #f 0 #f '() #f)))
  (%ed-on-submit-set! d "sub1")
  (check (%ed-on-submit d) => "sub1")
) ;let

(check-report)
