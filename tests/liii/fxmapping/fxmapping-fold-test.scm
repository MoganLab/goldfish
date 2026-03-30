(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-fold
;; 左折叠遍历映射（按键升序）。
;;
;; 语法
;; ----
;; (fxmapping-fold proc nil fxmap)
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
(check (fxmapping-fold (lambda (k v acc) (+ v acc)) 0 (fxmapping 0 10 1 20 2 30)) => 60)
(check (fxmapping-fold (lambda (k v acc) (cons k acc)) '() (fxmapping 0 'a 1 'b 2 'c)) => '(2 1 0))

(check-report)
