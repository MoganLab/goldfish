(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-xor
;; 对称差集操作。
;;
;; 语法
;; ----
;; (fxmapping-xor fxmap1 fxmap2)
;;
;; 参数
;; ----
;; fxmap1, fxmap2 : fxmapping
;; 要计算对称差集的映射。
;;
;; 返回值
;; -----
;; 返回只包含在恰好一个映射中存在的键的新 fxmapping。
;;
(let ((xor (fxmapping-xor (fxmapping 0 'a 1 'b) (fxmapping 1 'B 2 'c))))
  (check-true (fxmapping-contains? xor 0))
  (check-false (fxmapping-contains? xor 1))
  (check-true (fxmapping-contains? xor 2))
) ;let

(check-report)
