(import (liii check) (liii tui editor view))

(check-set-mode! 'report-failed)

;; render-line-with-cursor
;; 在单行文本的指定位置绘制软件光标。
;;
;; 语法
;; ----
;; (render-line-with-cursor line col focused? cursor-style)
;;
;; 参数
;; ----
;; line : string?
;; 待渲染的单行文本。
;;
;; col : integer?
;; 光标的显示列位置，从 0 开始（宽字符占多列，显示列与字符索引可能不同）。
;;
;; focused? : boolean?
;; 编辑器是否处于聚焦状态。
;;
;; cursor-style : symbol?
;; 光标样式，支持 inverse、bar、block 和 hardware。
;;
;; 返回值
;; ----
;; string
;; 添加光标指示后的文本。未聚焦或使用 hardware 样式时返回原文本。

;; 未聚焦时原样返回
(check (render-line-with-cursor "hello" 2 #f 'inverse) => "hello")

;; 聚焦时反显行内字符或行尾空格
(check (render-line-with-cursor "hello" 1 #t 'inverse)
  =>
  "h\x1b;[7me\x1b;[0mllo"
) ;check
(check (render-line-with-cursor "hello" 5 #t 'inverse)
  =>
  "hello\x1b;[7m \x1b;[0m"
) ;check

;; 竖线光标样式
(check (render-line-with-cursor "hello" 2 #t 'bar) => "he|llo")

;; 中文字符按显示列定位：显示列 2 是“好”的起点（宽字符占 2 列）
(check (render-line-with-cursor "你好世界" 2 #t 'inverse)
  =>
  "你\x1b;[7m好\x1b;[0m世界"
) ;check
(check (render-line-with-cursor "你好世界" 2 #t 'bar) => "你|好世界")
(check (render-line-with-cursor "你好" 4 #t 'inverse)
  =>
  "你好\x1b;[7m \x1b;[0m"
) ;check

;; 光标位于行尾空格之后：显示列超出行的显示宽度时补空格再反显
;; （wrap-text 会去掉行尾空格，渲染行是 trim 后的文本，
;;   软件方块光标必须能画到被 trim 掉的空格位置，Issue 1218）
(check (render-line-with-cursor "hello" 6 #t 'inverse)
  =>
  "hello \x1b;[7m \x1b;[0m"
) ;check
(check (render-line-with-cursor "abc" 8 #t 'inverse)
  =>
  "abc     \x1b;[7m \x1b;[0m"
) ;check
(check (render-line-with-cursor "hello" 6 #t 'bar) => "hello |") ;check
;; 行中空格之后的显示列（光标前有空格但行未折行）
(check (render-line-with-cursor "a b" 3 #t 'inverse)
  =>
  "a b\x1b;[7m \x1b;[0m"
) ;check
;; 中文后接行尾空格列：显示列 5 超出“你好”的显示宽 4 → 补 1 空格
;; （补位规则与 ASCII 一致：行内容宽度补到光标列，光标块落在该列）
(check (render-line-with-cursor "你好" 5 #t 'inverse)
  =>
  "你好 \x1b;[7m \x1b;[0m"
) ;check
;; 显示列 6 超出“你好”的显示宽 4 → 补 2 空格
(check (render-line-with-cursor "你好" 6 #t 'inverse)
  =>
  "你好  \x1b;[7m \x1b;[0m"
) ;check

;; 硬件光标样式原样返回行内容
(check (render-line-with-cursor "hello" 2 #t 'hardware) => "hello")
(check (render-line-with-cursor "你好世界" 2 #t 'hardware) => "你好世界")

;; 显示列落在宽字符中间：防御性对齐到该宽字符（正常路径不会出现，
;; editor-visual-cursor 产出的显示列总在字符边界）
(check (render-line-with-cursor "你好世界" 3 #t 'inverse)
  =>
  "你\x1b;[7m好\x1b;[0m世界"
) ;check

(check-report)
