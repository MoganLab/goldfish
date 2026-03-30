(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-union/combinator
;; 并集操作，使用自定义合并函数处理重复键。
;;
;; 语法
;; ----
;; (fxmapping-union/combinator combiner fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; combiner : procedure
;; 合并函数，接收 key、value1、value2，返回合并后的值。
;;
;; fxmap1, fxmap2, ... : fxmapping
;; 要合并的映射。
;;
;; 返回值
;; -----
;; 返回包含所有映射中所有键的新 fxmapping。
;; 对于重复键，使用 combiner 合并值。
;;
(let ((union (fxmapping-union/combinator (lambda (k v1 v2) (+ v1 v2))
                                         (fxmapping 0 10 1 20)
                                         (fxmapping 1 5 2 30))))
  (check (fxmapping-ref union 0 (lambda () 'not-found)) => 10)
  (check (fxmapping-ref union 1 (lambda () 'not-found)) => 25)
  (check (fxmapping-ref union 2 (lambda () 'not-found)) => 30)
) ;let

(check-report)