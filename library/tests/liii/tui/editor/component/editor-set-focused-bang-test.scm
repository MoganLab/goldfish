;; editor-set-focused!
;; 设置编辑器的聚焦状态。
;;
;; 语法
;; ----
;; (editor-set-focused! ed focused?)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; focused? : boolean?
;; 是否聚焦。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor)))
  (editor-set-focused! ed #f)
  (check-false (editor-focused? ed))
  (editor-set-focused! ed #t)
  (check-true (editor-focused? ed))
) ;let

(check-report)
