;; markdown-block-data-ref
;; 从 markdown-block 的 data 中按键取值。
;;
;; 语法
;; ----
;; (markdown-block-data-ref block key default)
;;
;; 参数
;; ----
;; block : markdown-block
;; 块记录。
;; key : symbol
;; 键。
;; default : any
;; 默认值。
;;
;; 返回值
;; ----
;; any
;; 返回字段值或默认值。

(import (scheme base))
(import (liii check))
(import (liii cmark block collector))

(check-set-mode! 'report-failed)

(let ((b (make-markdown-block 'heading '() '((level . 2)))))
  (check (markdown-block-data-ref b 'level 1) => 2)
  (check (markdown-block-data-ref b 'other 0) => 0)
) ;let

(check-report)
