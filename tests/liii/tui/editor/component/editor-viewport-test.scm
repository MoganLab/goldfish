;; editor-viewport
;; 获取编辑器当前视口的视觉行范围及上下隐藏行数。
;;
;; 语法
;; ----
;; (editor-viewport ed [width])
;;
;; 参数
;; ----
;; ed : editor?
;; 目标编辑器组件。
;;
;; width : integer? (可选)
;; 显示宽度。
;;
;; 返回值
;; ----
;; list?
;; 包含 (top visible above below) 的四元列表。

(import (scheme base) (liii check) (liii tui editor component))

(check-set-mode! 'report-failed)

(let ((ed (make-editor "hello")))
  (let ((vp (editor-viewport ed 40)))
    (check (list? vp) => #t)
    (check (length vp) => 4)
  ) ;let
) ;let

(check-report)
