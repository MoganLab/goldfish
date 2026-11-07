;; pi-budget-warning
;; 判断 Agent 当前轮次是否触发预算临界预警，仅临界时返回静态告警块。
;;
;; 设计动机：常规的轮数/剩余时间状态每轮都变化，若拼进系统提示词会击穿
;; 供应商的前缀缓存（prompt cache）。因此只有触发临界条件（剩余轮数 <= 3、
;; 剩余时间 < 60s 或 < 10%）时才返回内容，且告警文本为静态字符串，
;; 保证进入临界区后系统提示词重新稳定。
;;
;; 语法
;; ----
;; (pi-budget-warning turn max-turns remaining-sec deadline-sec session-timeout)
;;
;; 参数
;; ----
;; turn : integer?，当前轮次数
;; max-turns : (or integer? #f)，最大轮数上限，无限制时为 #f 或 0
;; remaining-sec : (or real? #f)，会话剩余秒数
;; deadline-sec : (or real? #f)，会话截止时间戳，无限制时为 #f
;; session-timeout : (or real? #f)，会话超时总时长（秒）
;;
;; 返回值
;; ----
;; (or string? #f)，触发临界时返回 <budget_warning> 告警块字符串，否则 #f。

(import (liii check) (liii string) (liii pi agent budget))

(check-set-mode! 'report-failed)

;; 1. 无限制模式（无最大轮数，无时间限制）：返回 #f
(check (pi-budget-warning 1 #f #f #f #f) => #f)

;; 2. 正常充裕预算（剩余轮数充足且剩余时间充足）：返回 #f
(check (pi-budget-warning 2 10 300.0 1700000300 600) => #f)

;; 3. 轮数临界（剩余轮数 <= 3，时间充足）：返回轮数告警块
(let ((w (pi-budget-warning 8 10 300.0 1700000300 600)))
  (check-true (string? w))
  (check-true (string-contains w "<budget_warning>"))
  (check-true (string-contains w "WARNING: Approaching maximum turn limit (<= 3 turns left).")
  ) ;check-true
  (check-true (string-contains w "</budget_warning>"))
  (check-false (string-contains w "CRITICAL"))
) ;let

;; 4. 时间临界（剩余时间 < 60s 或 < 10%，轮数充足）：返回时间告警块
(let ((w (pi-budget-warning 1 10 45.0 1700000045 600)))
  (check-true (string? w))
  (check-true (string-contains w "<budget_warning>"))
  (check-true (string-contains w
                "WARNING: Approaching session timeout deadline (< 60s or < 10% time left)."
              ) ;string-contains
  ) ;check-true
  (check-false (string-contains w "CRITICAL"))
) ;let

;; 5. 双重临界：返回 CRITICAL 告警块
(let ((w (pi-budget-warning 8 10 30.0 1700000030 600)))
  (check-true (string? w))
  (check-true (string-contains w "CRITICAL BUDGET WARNING"))
  (check-true (string-contains w "</budget_warning>"))
  (check-false (string-contains w "Approaching"))
) ;let

;; 6. 轮数临界的判定边界：剩余 3 轮触发，剩余 4 轮不触发
(check (string? (pi-budget-warning 7 10 300.0 1700000300 600)) => #t)
(check (pi-budget-warning 6 10 300.0 1700000300 600) => #f)

;; 7. 时间临界的判定边界：恰好 60 秒不触发（阈值是 <），59 秒触发
(check (pi-budget-warning 1 10 60.0 1700000060 600) => #f)
(check (string? (pi-budget-warning 1 10 59.0 1700000059 600)) => #t)

;; 8. 时间临界阈值取 max(60, 10% * session-timeout)：session-timeout 为
;;    2000 秒时阈值 200 秒，150 秒剩余触发
(check (string? (pi-budget-warning 1 10 150.0 1700000150 2000)) => #t)
(check (pi-budget-warning 1 10 250.0 1700000250 2000) => #f)

;; 9. max-turns 为 0 视为无限制：即使剩 1 轮也不触发轮数告警
(check (pi-budget-warning 100 0 300.0 1700000300 600) => #f)

;; 10. deadline-sec 为 #f 时忽略时间维度（remaining-sec 缺省也不抛错）；
;;     轮数维度照常生效
(check (pi-budget-warning 6 10 #f #f #f) => #f)
(check (string? (pi-budget-warning 8 10 #f #f #f)) => #t)

;; 11. 告警文本为静态字符串：不包含具体轮数或秒数数字，保证临界区内
;;     逐轮内容一致，前缀缓存最多只断一次
(let ((w1 (pi-budget-warning 8 10 300.0 1700000300 600))
      (w2 (pi-budget-warning 9 10 300.0 1700000300 600))
     ) ;
  (check (string=? w1 w2) => #t)
) ;let

(check-report)
