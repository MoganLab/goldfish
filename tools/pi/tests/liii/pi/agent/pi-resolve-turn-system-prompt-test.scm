(import (liii check)
        (liii string)
        (liii pi agent loop)
) ;import

(check-set-mode! 'report-failed)

;; pi-resolve-turn-system-prompt
;; 1. 基础兼容性：字符串直接返回；过程以本回合工具调用为参数求值。
(check (pi-resolve-turn-system-prompt "base prompt" '()) => "base prompt")

(check (pi-resolve-turn-system-prompt
         (lambda (tool-calls) (string-append "base+" (number->string (length tool-calls))))
         '("a" "b")
       ) ;pi-resolve-turn-system-prompt
  => "base+2"
) ;check

;; 2. 回合级预算注入：正常充裕预算（第 1 轮，总 200 轮，剩余 500 秒 / 600 秒）
(let ((sp (pi-resolve-turn-system-prompt "base" '() 1 200 500.0 500.0 600)))
  (check-true (string-contains? sp "base"))
  (check-true (string-contains? sp "<current_turn_budget>"))
  (check-true (string-contains? sp "Turn Status: Turn 1 (out of max 200, 199 remaining)."))
  (check-true (string-contains? sp "Time Status: 500 seconds remaining before session timeout."))
  (check-false (string-contains? sp "<budget_warning>")))

;; 3. 轮次临界预警：剩余轮次 <= 3（第 198 轮 / 200 轮）
(let ((sp (pi-resolve-turn-system-prompt "base" '() 198 200 400.0 400.0 600)))
  (check-true (string-contains? sp "Turn Status: Turn 198 (out of max 200, 2 remaining)."))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp "WARNING: Approaching maximum turn limit (<= 3 turns left).")))

;; 4. 时间临界预警：剩余时间不足 60 秒或 < 10%（例如 45 秒）
(let ((sp (pi-resolve-turn-system-prompt "base" '() 5 200 45.0 45.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp "WARNING: Approaching session timeout deadline (< 60s or < 10% time left).")))

;; 5. 双重临界危机预警：轮次与时间双重不足
(let ((sp (pi-resolve-turn-system-prompt "base" '() 199 200 30.0 30.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp "CRITICAL BUDGET WARNING: You are nearly out of turns AND time!")))

;; 6. 无限轮次与无时间死线
(let ((sp (pi-resolve-turn-system-prompt "base" '() 10 0 #f #f #f)))
  (check-true (string-contains? sp "Turn Status: Turn 10 (unlimited turns)."))
  (check-true (string-contains? sp "Time Status: Unlimited (no session timeout)."))
  (check-false (string-contains? sp "<budget_warning>")))

(check-report)
