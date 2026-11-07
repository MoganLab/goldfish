;; pi-key-pool-invalid-cooldown-ms
;; 读取密钥池的密钥失效冷却时长。
;;
;; 语法
;; ----
;; (pi-key-pool-invalid-cooldown-ms pool)
;;
;; 参数
;; ----
;; pool : pi-key-pool?
;; 密钥池记录。
;;
;; 返回值
;; ----
;; number?
;; 冷却时长（毫秒）。

(import (liii check)
        (liii pi multikey pool)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认失效冷却时长
(check (pi-key-pool-invalid-cooldown-ms (pi-key-pool "google")) => 600000)

;; 2. 自定义失效冷却时长
(check (pi-key-pool-invalid-cooldown-ms (pi-key-pool "google" 1000 3000)) => 3000)

(check-report)
