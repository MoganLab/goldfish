;; editor-display-col
;; 获取编辑器光标在当前行上的终端物理显示列（考虑宽字符）。
;;
;; 语法
;; ----
;; (editor-display-col ed)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; 返回值
;; ----
;; integer?
;; 终端显示列索引。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "")))
  (check (editor-display-col ed) => 0)
) ;let

(let ((ed (make-editor "hello world")))
  (editor-set-cursor! ed 0 5)
  (check (editor-display-col ed) => 5)
) ;let

(check-report)
