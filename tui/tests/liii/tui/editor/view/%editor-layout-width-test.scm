;; %editor-layout-width
;; 计算编辑器文本布局的可用宽度（无内边距时预留最后一列给硬件光标）。
;;
;; 语法
;; ----
;; (%editor-layout-width width)
;;
;; 参数
;; ----
;; width : integer?
;; 原始可用宽度。
;;
;; 返回值
;; ----
;; integer?
;; 扣除光标预留列后的有效排版宽度。

(import (scheme base) (liii check) (liii tui editor view))

(check-set-mode! 'report-failed)

(check (%editor-layout-width 80) => 79)
(check (%editor-layout-width 10) => 9)
(check (%editor-layout-width 1) => 1)
(check (%editor-layout-width 0) => 0)

(check-report)
