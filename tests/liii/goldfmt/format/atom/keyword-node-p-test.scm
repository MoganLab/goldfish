(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; keyword-node?
;; 判断 node 是否为 keyword atom。
;;
;; 语法
;; ----
;; (keyword-node? node)
;;
;; 参数
;; ----
;; node : any?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：keyword atom 为 #t
(check (keyword-node? (make-atom :value #:key)) => #t)

;; 测试：symbol atom 为 #f
(check (keyword-node? (make-atom :value 'key)) => #f)

;; 测试：env 为 #f
(check (keyword-node? (make-env :tag-name "define")) => #f)

(check-report)
