;; editor-set-cursor!
;; 显式设置编辑器的光标位置。
;;
;; 语法
;; ----
;; (editor-set-cursor! ed row col)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; row : integer?
;; 逻辑行号（0-based）。
;;
;; col : integer?
;; 逻辑列号（0-based）。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "hello")))
  (editor-set-cursor! ed 0 2)
  (check (editor-cursor ed) => '(0 2))
) ;let

(check-report)
