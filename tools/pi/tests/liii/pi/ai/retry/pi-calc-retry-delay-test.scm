;; pi-calc-retry-delay
;; 计算指数退避重试延迟时间（支持服务端 Retry-After 指示）。
;;
;; 语法
;; ----
;; (pi-calc-retry-delay attempt base-delay-ms max-delay-ms)
;; (pi-calc-retry-delay attempt base-delay-ms max-delay-ms retry-after-sec)
;;
;; 参数
;; ----
;; attempt : integer?
;; 当前重试次数（从 1 开始）。
;;
;; base-delay-ms : number?
;; 基础延迟毫秒数。
;;
;; max-delay-ms : number?
;; 最大延迟毫秒数。
;;
;; retry-after-sec : number?
;; 可选，服务端指示的重试等待秒数。
;;
;; 返回值
;; ----
;; real?
;; 退避等待时间（秒）。

(import (scheme base) (liii check) (liii pi ai retry))

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
