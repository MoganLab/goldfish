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
;; 光标的显示列位置，从 0 开始。
;;
;; focused? : boolean?
;; 编辑器是否处于聚焦状态。
;;
;; cursor-style : symbol?
;; 光标样式，支持 inverse、bar、block 和 hardware。
;;
;; 返回值
;; ----
;; string?
;; 添加光标指示后的文本；未聚焦或使用 hardware 样式时返回原文本。

(import (scheme base) (liii check) (liii tui editor view))

(check-set-mode! 'report-failed)

;; 未聚焦时原样返回
(check (render-line-with-cursor "hello" 2 #f 'inverse) => "hello")

;; hardware 样式原样返回（由终端自身绘制硬件光标）
(check (render-line-with-cursor "hello" 2 #t 'hardware) => "hello")

(check-report)
