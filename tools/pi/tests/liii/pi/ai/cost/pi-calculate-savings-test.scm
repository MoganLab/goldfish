(import (scheme base)
  (liii check)
  (liii pi ai cost)
) ;import

(check-set-mode! 'report-failed)

;; Prompt Cache 节省金额计算：
;; 节省 = (base input rate - cacheRead rate) * cacheRead tokens / 1000000

;; 1. 命中 cacheRead 产生节省
(let* ((cost-rates '((input . 3.0) (output . 15.0) (cacheRead . 0.3) (cacheWrite . 0.0)))
       (usage '((input . 100000) (output . 5000) (cacheRead . 500000) (cacheWrite . 0)))
       (savings (pi-calculate-cache-savings cost-rates usage))
      ) ;let*
  ;; 节省单价: 3.0 - 0.3 = 2.7 每百万 token
  ;; cacheRead tokens: 500,000
  ;; 节省金额: 2.7 * 500000 / 1e6 = 1.35
  (check savings => 1.35)
) ;let

;; 2. 无 cacheRead 或 cacheRead 为 0
(let* ((cost-rates '((input . 3.0) (output . 15.0) (cacheRead . 0.3) (cacheWrite . 0.0)))
       (usage '((input . 100000) (output . 5000) (cacheRead . 0) (cacheWrite . 0)))
       (savings (pi-calculate-cache-savings cost-rates usage))
      ) ;let*
  (check savings => 0.0)
) ;let

;; 3. 累计多次 usage 的总 cost 与节省
(let* ((cost-rates '((input . 2.0) (output . 10.0) (cacheRead . 0.2) (cacheWrite . 0.0)))
       (usages (list
                 '((input . 100000) (output . 10000) (cacheRead . 100000) (cacheWrite . 0))
                 '((input . 50000) (output . 5000) (cacheRead . 200000) (cacheWrite . 0))))
       (summary (pi-summarize-session-cost cost-rates usages))
      ) ;let*
  ;; usage 1:
  ;; input = 0.2, output = 0.1, cacheRead = 0.02, total = 0.32
  ;; savings = (2.0 - 0.2) * 100000 / 1e6 = 0.18
  ;; usage 2:
  ;; input = 0.1, output = 0.05, cacheRead = 0.04, total = 0.19
  ;; savings = 1.8 * 200000 / 1e6 = 0.36
  ;; summary: totalCost = 0.51, totalSavings = 0.54
  (check (cdr (assoc 'totalCost summary)) => 0.51)
  (check (cdr (assoc 'totalSavings summary)) => 0.54)
) ;let

(check-report)
