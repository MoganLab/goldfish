(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-for-each
;; 遍历所有键值对执行副作用操作。
;;
;; 语法
;; ----
;; (fxmapping-for-each proc fxmap)
;;
;; 参数
;; ----
;; proc : procedure
;; 遍历函数，接收 key 和 value。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; 返回值
;; -----
;; 未指定返回值（用于副作用）。
;;
(let ((result '()))
  (fxmapping-for-each (lambda (k v) (set! result (cons (cons k v) result)))
                      (fxmapping 0 'a 1 'b 2 'c)
  ) ;fxmapping-for-each
  (check (length result) => 3)
) ;let

(check-report)
