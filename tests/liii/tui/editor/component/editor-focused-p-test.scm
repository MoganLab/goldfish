;; editor-focused?
;; 查询编辑器是否处于聚焦状态。
;;
;; 语法
;; ----
;; (editor-focused? ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; boolean?
;; 聚焦返回 #t，失焦返回 #f。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor)))
  (check-true (editor-focused? ed))
) ;let

(check-report)
