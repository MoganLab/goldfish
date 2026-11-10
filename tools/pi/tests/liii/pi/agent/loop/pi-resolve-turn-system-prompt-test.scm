;; pi-resolve-turn-system-prompt
;; 解析当前轮次的系统提示词：支持动态函数求值，仅在预算临界时附加静态告警块。
;;
;; 设计动机：系统提示词位于请求前缀的最前端，任何逐轮变化的内容（如轮数、
;; 剩余秒数）都会击穿供应商的前缀缓存。因此非临界时对任意入参返回字节级
;; 一致的 base；只有触发 <budget_warning>（剩余轮数 <= 3、剩余时间 < 60s
;; 或 < 10%）时才拼接一次静态告警文本，进入临界区后重新稳定。
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
;; string?，非临界时与 base 字节级一致；临界时为 base + 静态告警块。

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

;; 2. 前缀缓存稳定性（核心）：非临界时，任意 turn/max-turns/deadline 入参
;;    都必须返回与 base 字节级一致的系统提示词，不含任何预算状态文本
(check (pi-resolve-turn-system-prompt "base" '() 0 200 #f #f 600) => "base")
(check (pi-resolve-turn-system-prompt "base" '() 1 200 500.0 500.0 600)
  =>
  "base"
) ;check
(check (pi-resolve-turn-system-prompt "base" '() 50 200 100.0 100.0 600)
  =>
  "base"
) ;check
(check (pi-resolve-turn-system-prompt "base" '() 10 0 #f #f #f) => "base")
(let ((sp (pi-resolve-turn-system-prompt "base" '() 5 200 300.0 300.0 600)))
  (check-false (string-contains? sp "<current_turn_budget>"))
  (check-false (string-contains? sp "Turn Status"))
  (check-false (string-contains? sp "Time Status"))
) ;let

;; 3. turn 为 #f（非 Agent 循环调用）时直接返回 base
(check (pi-resolve-turn-system-prompt "base" '() #f 200 100.0 100.0 600)
  =>
  "base"
) ;check

;; 4. 轮次临界预警：剩余轮次 <= 3 时拼接静态告警块，且无动态数字
(let ((sp (pi-resolve-turn-system-prompt "base" '() 198 200 400.0 400.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "WARNING: Approaching maximum turn limit (<= 3 turns left)."
              ) ;string-contains?
  ) ;check-true
  (check-false (string-contains? sp "Turn Status"))
  (check-false (string-contains? sp "Time Status"))
) ;let

;; 5. 时间临界预警：剩余时间不足 60 秒或 < 10% 时拼接静态告警块
(let ((sp (pi-resolve-turn-system-prompt "base" '() 5 200 45.0 45.0 600)))
  (check-true (string-contains? sp "<budget_warning>"))
  (check-true (string-contains? sp
                "WARNING: Approaching session timeout deadline (< 60s or < 10% time left)."
              ) ;string-contains?
  ) ;check-true
) ;let

;; 6. 双重临界危机预警：轮次与时间双重不足
(let ((sp (pi-resolve-turn-system-prompt "base" '() 199 200 30.0 30.0 600)))
  (check-true (string-contains? sp "CRITICAL BUDGET WARNING"))
) ;let

;; 7. 临界区内逐轮稳定：告警文本为静态字符串，相邻两轮输出一致，
;;    前缀缓存最多在进入临界区时断一次
(check (string=? (pi-resolve-turn-system-prompt "base" '() 198 200 100.0 100.0 600)
         (pi-resolve-turn-system-prompt "base" '() 199 200 100.0 100.0 600)
       ) ;string=?
  =>
  #t
) ;check

(check-report)
