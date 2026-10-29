(import (liii check) (liii tui editor view))

(check-set-mode! 'report-failed)

;; editor-visual-cursor
;; 将编辑器的逻辑光标位置转换为折行后的视觉行和显示列。
;;
;; 语法
;; ----
;; (editor-visual-cursor lines row col width)
;;
;; 参数
;; ----
;; lines : (list string ...)
;; 编辑器的完整逻辑行列表。
;;
;; row : integer?
;; 光标所在的逻辑行，从 0 开始。
;;
;; col : integer?
;; 光标在逻辑行内的字符列，从 0 开始。
;;
;; width : integer?
;; 折行使用的最大显示宽度。
;;
;; 返回值
;; ----
;; (list integer? integer?)
;; 二元列表 (visual-row display-col)，分别表示从 0 开始的视觉行和显示列。
;;
;; 注意
;; ----
;; 视觉行号累计目标逻辑行之前的所有折行；显示列按终端显示宽度计算，
;; 因此中文字符的光标位置与字符索引可能不同。

;; 折行后的视觉光标坐标按完整逻辑行和行内前缀计算
(check (editor-visual-cursor '("hello world" "next") 1 2 6) => '(2 2))
(check (editor-visual-cursor '("你好世界") 0 3 4) => '(1 2))

;; 折行后光标紧跟最后一段行尾空格时，显示列需补回被 wrap-text 丢弃的空格
(check (editor-visual-cursor '("aaaa bbbb cccc dddd eeee ") 0 26 20) => '(1 5))

;; 折行后光标位于行中分隔空格之后、后续词之前，同样补回空格宽度
(check (editor-visual-cursor '("aaaa bbbb cccc dddd eeee ff") 0 25 20) => '(1 5))

;; 光标前缀达到显示宽度且后面还有内容时，光标移至下一视觉行行首
(check (editor-visual-cursor '("ab cd ef gh") 0 9 6) => '(1 3))

;; 行尾空格填满最后显示行时，光标贴当前视觉行最后一列
(check (editor-visual-cursor '("ab cd ef      ") 0 14 6) => '(2 5))

(check-report)
