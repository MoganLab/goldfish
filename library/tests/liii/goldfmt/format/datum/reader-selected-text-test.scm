;; reader-selected-text
;; 读取选中项中的文本。
;;
;; 语法
;; ----
;; (reader-selected-text item)
;;
;; 参数
;; ----
;; item : list?
;; 选中项，元素为 (表单 列号 文本)。
;;
;; 返回值
;; ----
;; string?
;; 选中项的文本内容。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(let ((sel (list 'item 5 "text")))
  (check (reader-selected-text sel) => "text")
) ;let

(check-report)
