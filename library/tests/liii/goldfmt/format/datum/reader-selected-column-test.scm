;; reader-selected-column
;; 读取选中项中的目标列号。
;;
;; 语法
;; ----
;; (reader-selected-column item)
;;
;; 参数
;; ----
;; item : list?
;; 选中项，元素为 (表单 列号 文本)。
;;
;; 返回值
;; ----
;; integer?
;; 选中项的目标列号。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(let ((sel (list 'item 5 "text")))
  (check (reader-selected-column sel) => 5)
) ;let

(check-report)
