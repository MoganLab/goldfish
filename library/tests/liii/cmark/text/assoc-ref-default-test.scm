;; assoc-ref-default
;; 在 alist 中按键查找值，找不到时返回默认值。
;;
;; 语法
;; ----
;; (assoc-ref-default alist key default)
;;
;; 参数
;; ----
;; alist : list
;; 关联列表。
;; key : any
;; 查找键。
;; default : any
;; 默认返回值。
;;
;; 返回值
;; ----
;; any
;; 找到返回对应 cdr，否则返回 default。

(import (scheme base))
(import (liii check))
(import (liii cmark text))

(check-set-mode! 'report-failed)

(check (assoc-ref-default '((a . 1) (b . 2)) 'b 0) => 2)
(check (assoc-ref-default '((a . 1)) 'z 99) => 99)

(check-report)
