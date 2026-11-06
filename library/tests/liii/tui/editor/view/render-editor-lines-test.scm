;; render-editor-lines
;; 按指定宽度渲染编辑器的逻辑行，并在光标所在位置应用软件光标样式。
;;
;; 语法
;; ----
;; (render-editor-lines lines row col focused? placeholder width cursor-style)
;;
;; 参数
;; ----
;; lines : list?
;; 编辑器的逻辑行列表。
;;
;; row : integer?
;; 光标所在的逻辑行（从 0 开始）。
;;
;; col : integer?
;; 光标在逻辑行内的字符列（从 0 开始）。
;;
;; focused? : boolean?
;; 编辑器是否处于聚焦状态。
;;
;; placeholder : string?
;; 内容为空且未聚焦时显示的占位文本。
;;
;; width : integer?
;; 每行的最大显示宽度。
;;
;; cursor-style : symbol?
;; 光标样式，支持 inverse、bar、block 和 hardware。
;;
;; 返回值
;; ----
;; list?
;; 渲染得到的文本行字符串列表。

(import (scheme base) (liii check) (liii tui text-width) (liii tui editor view))

(check-set-mode! 'report-failed)

;; 占位符渲染（未聚焦且内容为空）
(check (render-editor-lines '("") 0 0 #f "Type here..." 20 'block)
  =>
  '("Type here...")
) ;check

;; 硬件光标模式（不绘制软件光标反显，直接输出文本）
(check (render-editor-lines '("hello") 0 2 #t "" 20 'hardware) => '("hello"))

(check-report)
