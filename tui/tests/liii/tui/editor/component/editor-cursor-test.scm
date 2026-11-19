;; editor-cursor
;; 获取编辑器当前的光标位置列表 (row col)。
;;
;; 语法
;; ----
;; (editor-cursor ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; list?
;; 包含 (row col) 的双元素列表。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "hello")))
  (check (editor-cursor ed) => '(0 5))
) ;let

(check-report)
