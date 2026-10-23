(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record) (liii goldfmt scan))

(check-set-mode! 'report-failed)

;; quote-env?
;; 判断 node 是否为 quote 表达式节点（tag 为 quote 或 #_quote），
;; stem-mode 下恒为 #f。
;;
;; 语法
;; ----
;; (quote-env? node)
;;
;; 参数
;; ----
;; node : any?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：tag 为 quote 的 env 为 #t
(check (quote-env? (make-env :tag-name "quote")) => #t)

;; 测试：tag 为 #_quote 的 env 为 #t
(check (quote-env? (make-env :tag-name "#_quote")) => #t)

;; 测试：其他 tag 为 #f
(check (quote-env? (make-env :tag-name "define")) => #f)

;; 测试：atom 为 #f
(check (quote-env? (make-atom :value 'quote)) => #f)

;; 测试：stem-mode 下恒为 #f
(check (call-with-stem-mode (lambda () (quote-env? (make-env :tag-name "quote")))) => #f)

(check-report)
