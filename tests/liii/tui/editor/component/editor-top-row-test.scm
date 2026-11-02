;; editor-top-row
;; 获取编辑器视口当前滚动的起始行索引（0-based）。
;;
;; 语法
;; ----
;; (editor-top-row ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; integer?
;; 视口起始行号。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor)))
  (check (editor-top-row ed) => 0)
) ;let

(check-report)
