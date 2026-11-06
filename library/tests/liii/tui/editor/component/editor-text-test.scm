;; editor-text
;; 获取编辑器当前的完整文本内容。
;;
;; 语法
;; ----
;; (editor-text ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; string?
;; 拼接后的文本内容。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "hello")))
  (check (editor-text ed) => "hello")
) ;let

(check-report)
