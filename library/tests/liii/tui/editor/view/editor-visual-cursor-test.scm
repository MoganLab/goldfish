;; editor-visual-cursor
;; 将编辑器的逻辑光标位置转换为折行后的视觉行和显示列。
;;
;; 语法
;; ----
;; (editor-visual-cursor lines row col width)
;;
;; 参数
;; ----
;; lines : list?
;; 编辑器的完整逻辑行列表。
;;
;; row : integer?
;; 光标所在的逻辑行（从 0 开始）。
;;
;; col : integer?
;; 光标在逻辑行内的字符列（从 0 开始）。
;;
;; width : integer?
;; 折行使用的最大显示宽度。
;;
;; 返回值
;; ----
;; list?
;; 二元列表 (visual-row display-col)，分别表示从 0 开始的视觉行和显示列。

(import (scheme base) (liii check) (liii tui editor view))

(check-set-mode! 'report-failed)

;; 单行无折行：视觉行就是逻辑行，显示列等于字符列
(check (editor-visual-cursor '("hello") 0 2 20) => '(0 2))

;; 中文宽字符：字符列 1 对应显示列 2
(check (editor-visual-cursor '("你好世界") 0 1 20) => '(0 2))

;; 多行无折行：逻辑行累加为视觉行
(check (editor-visual-cursor '("first" "second") 1 3 20) => '(1 3))

(check-report)
