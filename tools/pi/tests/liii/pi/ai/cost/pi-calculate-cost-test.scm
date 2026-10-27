(import (scheme base)
  (liii check)
  (liii pi ai cost)
) ;import

(check-set-mode! 'report-failed)

;; pi-calculate-cost
;; 按每百万 token 费率计算各项 token 费用及总费用。
;;
;; 语法
;; ----
;; (pi-calculate-cost cost-rates usage)
;;
;; 参数
;; ----
;; cost-rates : alist
;; input、output、cacheRead、cacheWrite 费率及可选 tiers。
;; usage : alist
;; 各类 token 数量。
;;
;; 返回值
;; ----
;; alist
;; 包含 input、output、cacheRead、cacheWrite 和 total 费用。
;;
;; 说明
;; ----
;; inputTokensAbove 使用严格大于判断。
;; 多个 tiers 无序时选择已触发阈值最大的档位。
;;
;; 示例
;; ----
;; (cdr (assoc 'total (pi-calculate-cost rates usage))) => 0.55

;; 1. 基本计算：无 cache 无 tier
(let* ((cost-rates '((input . 1.0) (output . 2.0) (cacheRead . 0.1) (cacheWrite . 0.5)))
       (usage '((input . 1000000) (output . 500000) (cacheRead . 0) (cacheWrite . 0)))
       (cost (pi-calculate-cost cost-rates usage))
      ) ;let*
  (check-true (pair? cost))
  (check (cdr (assoc 'input cost)) => 1.0)
  (check (cdr (assoc 'output cost)) => 1.0)
  (check (cdr (assoc 'cacheRead cost)) => 0.0)
  (check (cdr (assoc 'cacheWrite cost)) => 0.0)
  (check (cdr (assoc 'total cost)) => 2.0)
) ;let

;; 2. 含 cacheRead 与 cacheWrite
(let* ((cost-rates '((input . 2.0) (output . 8.0) (cacheRead . 0.5) (cacheWrite . 1.0)))
       (usage '((input . 200000) (output . 100000) (cacheRead . 500000) (cacheWrite . 100000)))
       (cost (pi-calculate-cost cost-rates usage))
      ) ;let*
  ;; input = 2.0 * 200000 / 1e6 = 0.4
  ;; output = 8.0 * 100000 / 1e6 = 0.8
  ;; cacheRead = 0.5 * 500000 / 1e6 = 0.25
  ;; cacheWrite = 1.0 * 100000 / 1e6 = 0.1
  ;; total = 0.4 + 0.8 + 0.25 + 0.1 = 1.55
  (check (cdr (assoc 'input cost)) => 0.4)
  (check (cdr (assoc 'output cost)) => 0.8)
  (check (cdr (assoc 'cacheRead cost)) => 0.25)
  (check (cdr (assoc 'cacheWrite cost)) => 0.1)
  (check (cdr (assoc 'total cost)) => 1.55)
) ;let

;; 3. 跨阶梯 tier 计算 (以 Gemini 2.5 Pro 为例，>200k tokens 触发更高费率)
(let* ((cost-rates '((input . 1.25)
                     (output . 10.0)
                     (cacheRead . 0.125)
                     (cacheWrite . 0.0)
                     (tiers . (((inputTokensAbove . 200000)
                                (input . 2.5)
                                (output . 15.0)
                                (cacheRead . 0.25)
                                (cacheWrite . 0.0))))))
       ;; 总输入 token = input + cacheRead + cacheWrite = 150000 + 100000 = 250000 > 200000
       (usage '((input . 150000) (output . 10000) (cacheRead . 100000) (cacheWrite . 0)))
       (cost (pi-calculate-cost cost-rates usage))
      ) ;let*
  ;; 命中 tier: input=2.5, output=15.0, cacheRead=0.25
  ;; input: 2.5 * 150000 / 1e6 = 0.375
  ;; output: 15.0 * 10000 / 1e6 = 0.15
  ;; cacheRead: 0.25 * 100000 / 1e6 = 0.025
  ;; total: 0.375 + 0.15 + 0.025 = 0.55
  (check (cdr (assoc 'input cost)) => 0.375)
  (check (cdr (assoc 'output cost)) => 0.15)
  (check (cdr (assoc 'cacheRead cost)) => 0.025)
  (check (cdr (assoc 'total cost)) => 0.55)
) ;let

;; 4. tiers 顺序不影响结果：选择满足条件的最大 inputTokensAbove 档位
(let* ((cost-rates '((input . 1.0)
                     (output . 2.0)
                     (cacheRead . 0.1)
                     (cacheWrite . 0.0)
                     (tiers . (((inputTokensAbove . 100)
                                (input . 10.0)
                                (output . 20.0)
                                (cacheRead . 1.0)
                                (cacheWrite . 0.0))
                               ((inputTokensAbove . 10)
                                (input . 5.0)
                                (output . 10.0)
                                (cacheRead . 0.5)
                                (cacheWrite . 0.0))))))
       (usage '((input . 1000) (output . 0) (cacheRead . 0) (cacheWrite . 0)))
       (cost (pi-calculate-cost cost-rates usage)))
  (check (cdr (assoc 'input cost)) => 0.01)
  (check (cdr (assoc 'total cost)) => 0.01))

(check-report)
