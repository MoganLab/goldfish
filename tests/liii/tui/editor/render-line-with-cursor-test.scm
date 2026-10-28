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
;; 光标的字符列位置，从 0 开始。
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

;; 中文字符的反显与竖线光标样式
(check (render-line-with-cursor "你好世界" 1 #t 'inverse)
  =>
  "你\x1b;[7m好\x1b;[0m世界"
) ;check
(check (render-line-with-cursor "你好世界" 1 #t 'bar) => "你|好世界")
(check (render-line-with-cursor "你好" 2 #t 'inverse)
  =>
  "你好\x1b;[7m \x1b;[0m"
) ;check

;; 硬件光标样式原样返回行内容
(check (render-line-with-cursor "hello" 2 #t 'hardware) => "hello")
(check (render-line-with-cursor "你好世界" 2 #t 'hardware) => "你好世界")

(check-report)
