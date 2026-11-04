;; make-dotted-tail
;; 创建一个带点尾部的记录，用于表示形如 (a . b) 的结构。
;;
;; 语法
;; ----
;; (make-dotted-tail [form])
;;
;; 参数
;; ----
;; form : any? 可选，默认值为 '()
;; 点尾部所指向的 form。
;;
;; 返回值
;; ----
;; dotted-tail?
;; 新创建的点尾部记录。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

(let ((d (make-dotted-tail 'rest)))
  (check (dotted-tail? d) => #t)
  (check (dotted-tail-form d) => 'rest)
) ;let

;; 测试默认参数
(check (dotted-tail-form (make-dotted-tail)) => '())

(check-report)
