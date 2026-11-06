;; reader-selected-item
;; 读取选中项中的表单内容。
;;
;; 语法
;; ----
;; (reader-selected-item item)
;;
;; 参数
;; ----
;; item : list?
;; 选中项，元素为 (表单 列号 文本)。
;;
;; 返回值
;; ----
;; any?
;; 选中项中的表单内容。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(let ((sel (list 'item 5 "text")))
  (check (reader-selected-item sel) => 'item)
) ;let

(check-report)
