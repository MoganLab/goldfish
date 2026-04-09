(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-intersection/combinator
;; 交集操作，使用自定义合并函数处理重复键。
;;
;; 语法
;; ----
;; (fxmapping-intersection/combinator combiner fxmap1 fxmap2 ...)
;;
;; 参数
;; ----
;; combiner : procedure
;; 合并函数，接收 key、value1、value2，返回合并后的值。
;;
;; fxmap1, fxmap2, ... : fxmapping
;; 要求交集的映射。
;;
;; 返回值
;; -----
;; 返回只包含在所有映射中都存在的键的新 fxmapping。
;; 对于重复键，使用 combiner 合并值。
;;
(let
  ((intersection
     (fxmapping-intersection/combinator (lambda (k v1 v2) (+ v1 v2))
                                        (fxmapping 0 10 1 20 2 30)
                                        (fxmapping 1 5 2 15 3 40))
     ) ;fxmapping-intersection/combinator
   ) ;intersection
  (check-false (fxmapping-contains? intersection 0))
  (check (fxmapping-ref intersection 1 (lambda () 'not-found)) => 25)
  (check (fxmapping-ref intersection 2 (lambda () 'not-found)) => 45)
  (check-false (fxmapping-contains? intersection 3))
) ;let

(check-report)