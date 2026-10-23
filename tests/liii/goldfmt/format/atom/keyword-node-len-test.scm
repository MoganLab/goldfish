(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; keyword-node-len
;; 计算 keyword atom 显示文本的长度。
;;
;; 语法
;; ----
;; (keyword-node-len node)
;;
;; 参数
;; ----
;; node : keyword-node? 为 #t 的节点
;;
;; 返回值
;; ------
;; integer?

;; 测试：keyword 文本长度
(check (keyword-node-len (make-atom :value #:key)) => 4)

;; 测试：长 keyword 文本长度
(check (keyword-node-len (make-atom :value #:abcdef)) => 7)

(check-report)
