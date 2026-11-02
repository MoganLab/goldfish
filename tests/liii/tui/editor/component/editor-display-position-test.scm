(import (liii check) (liii tui))

(check-set-mode! 'report-failed)

;; editor-display-position
;; 获取编辑器光标相对于当前视口的视觉位置。
;;
;; 语法
;; ----
;; (editor-display-position ed width)
;;
;; 参数
;; ----
;; ed : editor?
;; 目标编辑器组件。
;;
;; width : integer?
;; 用于折行和计算视口的显示宽度。
;;
;; 返回值
;; ----
;; (list integer? integer?)
;; 二元列表 (row col)，分别表示光标相对视口首行的视觉行偏移和显示列。
;;
;; 错误处理
;; ----
;; type-error 当 ed 不是编辑器时。

(let ((ed (make-editor "one two three four" :max-lines 2 :cursor-style 'hardware)))
  (component-render ed 7)
  (check (editor-display-position ed 7) => '(1 4))
) ;let

(check-catch 'type-error (editor-display-position #f 7))

(check-report)
