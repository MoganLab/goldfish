;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; Agent 轮数限制与执行时间预算的临界预警。
;;
;; 设计动机：常规的逐轮预算状态（当前轮数、剩余秒数）每轮都变化，拼进系统
;; 提示词会击穿供应商的前缀缓存（prompt cache）。本模块只在触发临界条件时
;; 返回静态告警文本（不含任何逐轮变化的数字），进入临界区后系统提示词重新
;; 稳定，前缀缓存最多只断一次。
(define-library (liii pi agent budget)
  (import (scheme base) (liii string) (liii list))
  (export pi-budget-warning)
  (begin

    ;; 轮数临界：剩余轮数 <= 3（max-turns 为 #f 或 0 视为无限制）
    (define (%turn-warning max-turns turn)
      (if (and (number? max-turns) (> max-turns 0) (<= (- max-turns turn) 3))
        (string-append "<budget_warning>\n"
          "WARNING: Approaching maximum turn limit (<= 3 turns left). "
          "Wrap up immediately and present final output." "\n</budget_warning>"
        ) ;string-append
        #f
      ) ;if
    ) ;define

    ;; 时间临界：剩余时间 < max(60s, 10% * session-timeout)
    (define (%time-warning deadline-sec remaining-sec session-timeout)
      (if
        (and deadline-sec
          (number? remaining-sec)
          (< remaining-sec (max 60 (* (or session-timeout 600) 0.1)))
        ) ;and
        (string-append "<budget_warning>\n"
          "WARNING: Approaching session timeout deadline (< 60s or < 10% time left). "
          "Finalize work and report results." "\n</budget_warning>"
        ) ;string-append
        #f
      ) ;if
    ) ;define

    (define (pi-budget-warning turn max-turns remaining-sec deadline-sec session-timeout)
      (let ((turn-w (%turn-warning max-turns turn))
            (time-w (%time-warning deadline-sec remaining-sec session-timeout))
           ) ;
        (cond ((and turn-w time-w)
               (string-append "<budget_warning>\n"
                 "CRITICAL BUDGET WARNING: You are nearly out of turns AND time! "
                 "Stop issuing new tool operations. "
                 "Provide final conclusions and summary immediately."
                 "\n</budget_warning>"
               ) ;string-append
              ) ;
              (turn-w turn-w)
              (time-w time-w)
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
