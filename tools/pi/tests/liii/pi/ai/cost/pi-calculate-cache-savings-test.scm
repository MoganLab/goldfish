(import (scheme base) (liii check) (liii pi ai cost))

(check-set-mode! 'report-failed)

;; pi-calculate-cache-savings
;; 计算 cacheRead 相比普通 input 费率节省的费用。
;;
;; 语法
;; ----
;; (pi-calculate-cache-savings cost-rates usage)
;;
;; 参数
;; ----
;; cost-rates : alist
;; input、cacheRead 等每百万 token 费率及可选 tiers。
;; usage : alist
;; 包含 cacheRead 等 token 数量的用量记录。
;;
;; 返回值
;; ----
;; number
;; 非负缓存节省费用，单位为与费率一致的货币单位。
;;
;; 说明
;; ----
;; 节省为 max(0, input费率-cacheRead费率) × cacheRead tokens / 1000000。
;; 无 cacheRead 时返回 0，阶梯费率选择与 pi-calculate-cost 一致。
;;
;; 示例
;; ----
;; (pi-calculate-cache-savings cost-rates usage) => 1.35

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
