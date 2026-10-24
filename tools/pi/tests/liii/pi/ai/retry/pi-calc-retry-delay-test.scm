(import (scheme base)
  (liii check)
  (liii pi ai retry)
)

(check-set-mode! 'report-failed)

;; 1. 无 retry-after 时回退标准指数退避（3 参数旧接口）
(check (pi-calc-retry-delay 1 2000 60000) => 2.0)
(check (pi-calc-retry-delay 2 2000 60000) => 4.0)
(check (pi-calc-retry-delay 3 2000 60000) => 8.0)
(check (pi-calc-retry-delay 10 2000 60000) => 60.0)

;; 2. retry-after 为 #f 时同上
(check (pi-calc-retry-delay 1 2000 60000 #f) => 2.0)
(check (pi-calc-retry-delay 2 2000 60000 #f) => 4.0)

;; 3. 优先采用有效的 retry-after
(check (pi-calc-retry-delay 1 2000 60000 5.0) => 5.0)
(check (pi-calc-retry-delay 3 2000 60000 12.5) => 12.5)

;; 4. 下限钳制（至少 0.1 秒，防止 0 秒紧密死循环）
(check (pi-calc-retry-delay 1 2000 60000 0.0) => 0.1)
(check (pi-calc-retry-delay 1 2000 60000 0.05) => 0.1)

;; 5. 上限钳制（不得超过 max-agent-delay-ms 对应的秒数）
(check (pi-calc-retry-delay 1 2000 60000 86400.0) => 60.0)
(check (pi-calc-retry-delay 1 2000 10000 30.0) => 10.0)

(check-report)
