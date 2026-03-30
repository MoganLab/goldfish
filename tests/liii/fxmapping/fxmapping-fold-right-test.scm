(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-fold-right
;; 右折叠遍历映射（按键降序）。
;;
;; 语法
;; ----
;; (fxmapping-fold-right proc nil fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 折叠函数，接收 key、value 和累积值，返回新累积值。
;;
;; nil : any
;; 初始累积值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 返回最终累积值。
;;
(check (fxmapping-fold-right (lambda (k v acc) (cons k acc)) '() (fxmapping 0 'a 1 'b 2 'c)) => '(0 1 2))

(check-report)
