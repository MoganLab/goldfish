;; pi-resolve-turn-system-prompt
;; 解析当前轮次的系统提示词，支持动态函数求值及注入轮数与时间预算预警信息。
;;
;; 语法
;; ----
;; (pi-resolve-turn-system-prompt system-prompt tool-calls (turn #f) (max-turns #f) (remaining-sec #f) (deadline-sec #f) (session-timeout #f))
;;
;; 参数
;; ----
;; system-prompt : (or string? procedure?)，系统提示词模板或求值过程。
;; tool-calls : list?，当前轮次工具调用列表。
;; turn : (or integer? #f)，当前轮数。
;; max-turns : (or integer? #f)，最大轮数上限。
;; remaining-sec : (or real? #f)，剩余秒数。
;; deadline-sec : (or real? #f)，截止时间戳。
;; session-timeout : (or real? #f)，会话超时总时长。
;;
;; 返回值
;; ----
;; string?，解析并附加了预算状态的最终系统提示词字符串。

(import (liii check) (liii string) (liii pi agent loop))

(check-set-mode! 'report-failed)

;; 1. 基础兼容性：字符串直接返回；过程以本回合工具调用为参数求值。
(check (pi-resolve-turn-system-prompt "base prompt" '()) => "base prompt")

(check
  (pi-resolve-turn-system-prompt
    (lambda (tool-calls)
      (string-append "base+" (number->string (length tool-calls)))
    ) ;lambda
    '("a" "b")
  ) ;pi-resolve-turn-system-prompt
  =>
  "base+2"
) ;check

;; 2. 回合级预算注入：正常充裕预算（第 1 轮，总 200 轮，剩余 500 秒 / 600 秒）
(let ((sp (pi-resolve-turn-system-prompt "base" '() 1 200 500.0 500.0 600)))
  (check-true (string-contains? sp "base"))
  (check-true (string-contains? sp "<current_turn_budget>"))
  (check-true (string-contains? sp "Turn Status: Turn 1 (out of max 200, 199 remaining).")
  ) ;check-true
  (check-true (string-contains? sp
                "Time Status: 500 seconds remaining before session timeout."
              ) ;string-contains?
  ) ;check-true
  (check-false (string-contains? sp "<budget_warning>"))
) ;let

;; 3. 轮次临界预警：剩余轮次 <= 3（第 198 轮 / 200 轮）
(let ((sp (pi-resolve-turn-system-prompt "base" '() 198 200 400.0 400.0 600)))
  (check-true (string-contains? sp "Turn Status: Turn 198 (out of max 200, 2 remaining).")
  ) ;check-true
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "WARNING: Approaching maximum turn limit (<= 3 turns left)."
              ) ;string-contains?
  ) ;check-true
) ;let

;; 4. 时间临界预警：剩余时间不足 60 秒或 < 10%（例如 45 秒）
(let ((sp (pi-resolve-turn-system-prompt "base" '() 5 200 45.0 45.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "WARNING: Approaching session timeout deadline (< 60s or < 10% time left)."
              ) ;string-contains?
  ) ;check-true
) ;let

;; 5. 双重临界危机预警：轮次与时间双重不足
(let ((sp (pi-resolve-turn-system-prompt "base" '() 199 200 30.0 30.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "CRITICAL BUDGET WARNING: You are nearly out of turns AND time!"
              ) ;string-contains?
  ) ;check-true
) ;let

;; 6. 无限轮次与无时间死线
(let ((sp (pi-resolve-turn-system-prompt "base" '() 10 0 #f #f #f)))
  (check-true (string-contains? sp "Turn Status: Turn 10 (unlimited turns)."))
  (check-true (string-contains? sp "Time Status: Unlimited (no session timeout).")
  ) ;check-true
  (check-false (string-contains? sp "<budget_warning>"))
) ;let

(check-report)
