;; pi-format-turn-budget-status
;; 格式化 Agent 当前轮次的时间与轮数预算状态文本，包含预警提示。
;;
;; 语法
;; ----
;; (pi-format-turn-budget-status turn max-turns remaining-sec deadline-sec session-timeout)
;;
;; 参数
;; ----
;; turn : integer?，当前轮次数
;; max-turns : (or integer? #f)，最大轮数上限，无限制时为 #f
;; remaining-sec : (or real? #f)，会话剩余秒数
;; deadline-sec : (or real? #f)，会话截止时间戳，无限制时为 #f
;; session-timeout : (or real? #f)，会话超时总时长（秒）
;;
;; 返回值
;; ----
;; string?，格式化后的预算提示块，当接近上限时包含 <budget_warning> 警告信息。

(import (liii check) (liii string) (liii pi agent budget))

(check-set-mode! 'report-failed)

;; 1. 无限制模式（无最大轮数，无时间限制）
(let ((status (pi-format-turn-budget-status 1 #f #f #f #f)))
  (check-true (string-contains status "<current_turn_budget>"))
  (check-true (string-contains status "- Turn Status: Turn 1 (unlimited turns)."))
  (check-true (string-contains status "- Time Status: Unlimited (no session timeout).")
  ) ;check-true
  (check-true (string-contains status "</current_turn_budget>"))
  (check-false (string-contains status "<budget_warning>"))
) ;let

;; 2. 正常有限预算模式（未触发预警：剩余轮数充足且剩余时间充足）
(let ((status (pi-format-turn-budget-status 2 10 300.0 1700000300 600)))
  (check-true (string-contains status "- Turn Status: Turn 2 (out of max 10, 8 remaining).")
  ) ;check-true
  (check-true (string-contains status
                "- Time Status: 300 seconds remaining before session timeout."
              ) ;string-contains
  ) ;check-true
  (check-false (string-contains status "<budget_warning>"))
) ;let

;; 3. 轮数预警（剩余轮数 <= 3，时间充足）
(let ((status (pi-format-turn-budget-status 8 10 300.0 1700000300 600)))
  (check-true (string-contains status "- Turn Status: Turn 8 (out of max 10, 2 remaining).")
  ) ;check-true
  (check-true (string-contains status "<budget_warning>"))
  (check-true (string-contains status
                "WARNING: Approaching maximum turn limit (<= 3 turns left)."
              ) ;string-contains
  ) ;check-true
) ;let

;; 4. 时间预警（剩余时间 < 60s 或 < 10%，轮数充足）
(let ((status (pi-format-turn-budget-status 1 10 45.0 1700000045 600)))
  (check-true (string-contains status "<budget_warning>"))
  (check-true (string-contains status
                "WARNING: Approaching session timeout deadline (< 60s or < 10% time left)."
              ) ;string-contains
  ) ;check-true
) ;let

;; 5. 双重危急预警（轮数与时间均接近上限）
(let ((status (pi-format-turn-budget-status 9 10 20.0 1700000020 600)))
  (check-true (string-contains status "<budget_warning>"))
  (check-true (string-contains status
                "CRITICAL BUDGET WARNING: You are nearly out of turns AND time!"
              ) ;string-contains
  ) ;check-true
) ;let

(check-report)
