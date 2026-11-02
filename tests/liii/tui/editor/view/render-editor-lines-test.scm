(import (liii check) (liii tui editor view))

(check-set-mode! 'report-failed)

;; render-editor-lines
;; 按指定宽度渲染编辑器的逻辑行，并在光标所在位置应用软件光标样式。
;;
;; 语法
;; ----
;; (render-editor-lines lines row col focused? placeholder width cursor-style)
;;
;; 参数
;; ----
;; lines : (list string ...)
;; 编辑器的逻辑行列表。
;;
;; row : integer?
;; 光标所在的逻辑行，从 0 开始。
;;
;; col : integer?
;; 光标在逻辑行内的字符列，从 0 开始。
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
;; (list string ...)
;; 折行并绘制光标后的视觉行列表。超宽内容按显示宽度折行。

;; 空内容且未聚焦时显示 placeholder
(check (render-editor-lines '("") 0 0 #f "Type here..." 40 'inverse)
  =>
  '("Type here...")
) ;check

;; 空内容但聚焦时显示光标
(check (render-editor-lines '("") 0 0 #t "Type here..." 40 'inverse)
  =>
  '("\x1b;[7m \x1b;[0m")
) ;check

;; 多行渲染时仅在光标所在行绘制光标
(let ((lines '("line 1" "line 2")))
  (check (render-editor-lines lines 1 0 #t "" 40 'inverse)
    =>
    '("line 1" "\x1b;[7ml\x1b;[0mine 2")
  ) ;check
) ;let

;; 超宽内容行按显示宽度折行，而非依赖终端硬换行
(check (render-editor-lines '("hello world") 0 0 #f "" 6 'hardware)
  =>
  '("hello" "world")
) ;check

;; 逗号分词的数字串在 token 边界换行，不拆开数字
(check (render-editor-lines '("12,34,56") 0 0 #f "" 5 'hardware)
  =>
  '("12,34" ",56")
) ;check

(check-report)
