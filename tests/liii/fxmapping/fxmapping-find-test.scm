(import (liii check)
        (liii fxmapping)
) ;import

(check-set-mode! 'report-failed)

;; fxmapping-find
;; 查找满足谓词的第一个键值对。
;;
;; 语法
;; ----
;; (fxmapping-find pred fxmap failure)
;; (fxmapping-find pred fxmap failure success)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接收 key 和 value，返回布尔值。
;;
;; fxmap : fxmapping
;; 目标映射。
;;
;; failure : procedure
;; 未找到时调用的无参过程。
;;
;; success : procedure (可选)
;; 找到时调用的双参过程，接收 key 和 value。
;;
;; 返回值
;; -----
;; 如果找到满足 pred 的键值对，返回 success 的结果（默认为两个值：key 和 value）；
;; 否则返回 failure 的结果。
;;
(let-values (((k v) (fxmapping-find (lambda (k v) (> k 5)) (fxmapping 3 'a 7 'b 10 'c) (lambda () (values #f #f)))))
  (check k => 7)
  (check v => 'b)
) ;let-values
(let-values (((k v) (fxmapping-find (lambda (k v) (> k 100)) (fxmapping 3 'a 7 'b) (lambda () (values #f #f)))))
  (check k => #f)
) ;let-values

(check-report)
