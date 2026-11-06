;; editor-set-text!
;; 整体设置编辑器的文本内容。
;;
;; 语法
;; ----
;; (editor-set-text! ed text)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; text : string?
;; 新文本。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "old")))
  (editor-set-text! ed "new")
  (check (editor-text ed) => "new")
) ;let

(check-report)
