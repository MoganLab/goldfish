(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-adjoin/combinator
;; 添加键值对到映射，使用自定义合并函数处理重复键。
;;
;; 语法
;; ----
;; (fxmapping-adjoin/combinator fxmap combiner key value ...)
;;
;; 参数
;; ----
;; fxmap : fxmapping
;; 目标映射。
;;
;; combiner : procedure
;; 合并函数，接收 key、new-value、old-value，返回合并后的值。
;;
;; key : exact-integer
;; 要添加的键。
;;
;; value : any
;; 关联的值。
;;
;; 返回值
;; -----
;; 返回新的 fxmapping。如果键已存在，使用 combiner 合并新旧值。
;;
(let ((m (fxmapping-adjoin/combinator (fxmapping 0 'a) (lambda (k new old) (list new old)) 0 'b)))
  (check (fxmapping-ref m 0 (lambda () 'not-found)) => '(b a))
) ;let

(let ((m (fxmapping-adjoin/combinator (fxmapping 0 'a) (lambda (k new old) old) 1 'b)))
  (check (fxmapping-ref m 0 (lambda () 'not-found)) => 'a)
  (check (fxmapping-ref m 1 (lambda () 'not-found)) => 'b)
) ;let

(check-report)
