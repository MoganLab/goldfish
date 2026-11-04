;; reader-keyword-block-info
;; 计算可以内联的连续 keyword 参数对信息。
;;
;; 语法
;; ----
;; (reader-keyword-block-info current rest-indent fmt-inline)
;;
;; 参数
;; ----
;; current : list?
;; 待检查的 keyword 参数列表。
;;
;; rest-indent : integer?
;; 续行的缩进列。
;;
;; fmt-inline : procedure?
;; 内联格式化过程。
;;
;; 返回值
;; ----
;; values
;; 两个值：可摆放的参数对数量与最大 key 长度。

(import (liii check) (liii goldfmt format datum) (liii goldfmt format reader))

(check-set-mode! 'report-failed)

(call-with-values (lambda ()
                    (reader-keyword-block-info (list :k1 'v1 :k2 'v2) 0 format-reader-datum-inline)
                  ) ;lambda
  (lambda (count max-key-len) (check count => 2) (check max-key-len => 3))
) ;call-with-values

(check-report)
