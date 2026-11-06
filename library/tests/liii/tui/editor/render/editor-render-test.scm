;; editor-render
;; 渲染编辑器组件，按给定宽度输出可显示的行列表。
;;
;; 语法
;; ----
;; (editor-render comp width)
;;
;; 参数
;; ----
;; comp : component?
;; 编辑器组件，由 make-editor、make-text-input 或 make-text-area 构造。
;;
;; width : integer?
;; 可用的显示宽度。
;;
;; 返回值
;; ----
;; list?
;; 渲染后的行列表，每个元素是一行显示文本。

(import (scheme base)
  (liii check)
  (liii tui component)
  (liii tui editor component)
  (liii tui editor render)
) ;import

(check-set-mode! 'report-failed)

(let ((ed (make-text-input "hello")))
  (check-true (pair? (editor-render ed 40)))
) ;let

(check-catch 'type-error (editor-render "not-an-editor" 40))

(check-report)
