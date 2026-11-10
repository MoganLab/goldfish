;; editor-placeholder
;; 获取编辑器的占位提示文本。
;;
;; 语法
;; ----
;; (editor-placeholder ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; string?
;; 占位提示文本。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor :placeholder "Type here...")))
  (check (editor-placeholder ed) => "Type here...")
) ;let

(check-report)
