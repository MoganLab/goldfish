;; editor-display-position
;; 获取编辑器光标相对于当前视口的视觉行偏移与显示列。
;;
;; 语法
;; ----
;; (editor-display-position ed width)
;;
;; 参数
;; ----
;; ed : editor?
;; 编辑器组件。
;;
;; width : integer?
;; 显示宽度。
;;
;; 返回值
;; ----
;; list?
;; 二元列表 (visual-row-offset display-col)。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "hello")))
  (check (editor-display-position ed 40) => '(0 5))
) ;let

(check-report)
