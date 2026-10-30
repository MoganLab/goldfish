(import (liii check) (liii tui component) (liii tui editor component) (liii tui editor render))

(check-set-mode! 'report-failed)

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
;; width : number?
;; 可用的显示宽度（单位为字符），用于换行与视口计算。
;;
;; 返回值
;; ----
;; (list string ...)
;; 渲染后的行列表，每个元素是一行显示文本。
;;
;; 说明
;; ----
;; 该函数是编辑器组件的渲染回调，由组件框架在重绘时调用；
;; 也可以直接调用它来检查渲染结果。
;; 设置了上边框或下边框时，首尾会追加水平边框行；
;; 内容超出可视区域时，首尾会追加滚动提示行（如 "↑ 3 more"）。
;; 光标样式为 hardware 时不绘制软件光标。
;;
;; 错误处理
;; ----
;; type-error 当 comp 不是组件时。

;; 1. 普通内容渲染（软件光标样式）
(let ((ed (make-editor "hi" :cursor-style 'bar)))
  (editor-set-cursor! ed 0 1)
  (check (editor-render ed 20) => '("h|i"))
) ;let

;; 2. 与组件框架的渲染结果一致
(let ((ed (make-editor "hi" :cursor-style 'bar)))
  (editor-set-cursor! ed 0 1)
  (check (editor-render ed 20) => (component-render ed 20))
) ;let

;; 3. 空编辑器渲染为单个光标行
(let ((ed (make-editor "" :cursor-style 'bar)))
  (check (editor-render ed 20) => '("|"))
) ;let

;; 4. 未聚焦时显示占位文本
(let ((ed (make-editor "" :placeholder "输入内容" :focused #f)))
  (check (editor-render ed 20) => '("输入内容"))
) ;let

;; 5. 上下边框各追加一行水平边框
(let ((ed (make-editor "hi" :cursor-style 'bar :top-border #t :bottom-border #t)))
  (let ((lines (editor-render ed 20)))
    (check (length lines) => 3)
    (check (list-ref lines 1) => "hi|")
    (check (string? (list-ref lines 0)) => #t)
    (check (string? (list-ref lines 2)) => #t)
  ) ;let
) ;let

;; 6. 宽度不足时自动换行（布局宽度 = 组件宽度 - 1，最后一列留给光标）
(let ((ed (make-editor "abcdef" :cursor-style 'bar)))
  (check (editor-render ed 3) => '("ab" "cd" "ef|"))
) ;let

(check-report)
