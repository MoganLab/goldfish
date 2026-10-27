(import (scheme base) (liii check) (liii pi ai cost))

(check-set-mode! 'report-failed)

;; 命中 cacheRead 产生节省
(let* ((cost-rates '((input . 3.0)
                     (output . 15.0)
                     (cacheRead . 0.3)
                     (cacheWrite . 0.0))
       ) ;cost-rates
       (usage '((input . 100000)
                (output . 5000)
                (cacheRead . 500000)
                (cacheWrite . 0))
       ) ;usage
       (savings (pi-calculate-cache-savings cost-rates usage))
      ) ;
  (check savings => 1.35)
) ;let*

;; 无 cacheRead 或 cacheRead 为 0
(let* ((cost-rates '((input . 3.0)
                     (output . 15.0)
                     (cacheRead . 0.3)
                     (cacheWrite . 0.0))
       ) ;cost-rates
       (usage '((input . 100000)
                (output . 5000)
                (cacheRead . 0)
                (cacheWrite . 0)))
       (savings (pi-calculate-cache-savings cost-rates usage))
      ) ;
  (check savings => 0.0)
) ;let*

(check-report)
