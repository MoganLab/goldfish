;; editor-set-placeholder!
;; 设置编辑器的占位提示文本。
;;
;; 语法
;; ----
;; (editor-set-placeholder! ed placeholder)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; placeholder : string?
;; 新的占位提示文本。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor)))
  (editor-set-placeholder! ed "New...")
  (check (editor-placeholder ed) => "New...")
) ;let

(check-report)
