;; pi-summarize-session-cost
;; 统计整个会话中所有请求的总费用与节省费用。
;;
;; 语法
;; ----
;; (pi-summarize-session-cost cost-rates usages)
;;
;; 参数
;; ----
;; cost-rates : alist
;; 模型的定价费率表。
;;
;; usages : list?
;; 各轮请求的用量记录列表。
;;
;; 返回值
;; ----
;; alist
;; 包含 totalCost 与 totalSavings 的统计结果关联列表。

(import (scheme base) (liii check) (liii pi ai cost))

(check-set-mode! 'report-failed)

;; 累计多次 usage 的总 cost 与节省
(let* ((cost-rates '((input . 2.0)
                     (output . 10.0)
                     (cacheRead . 0.2)
                     (cacheWrite . 0.0))
       ) ;cost-rates
       (usages (list '((input . 100000)
                       (output . 10000)
                       (cacheRead . 100000)
                       (cacheWrite . 0))
                 '((input . 50000)
                   (output . 5000)
                   (cacheRead . 200000)
                   (cacheWrite . 0))
               ) ;list
       ) ;usages
       (summary (pi-summarize-session-cost cost-rates usages))
      ) ;
  (check (cdr (assoc 'totalCost summary)) => 0.51)
  (check (cdr (assoc 'totalSavings summary)) => 0.54)
) ;let*

(check-report)
