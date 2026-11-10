;; pi-key-pool-cooldown-ms
;; 读取密钥池的普通限流冷却时长。
;;
;; 语法
;; ----
;; (pi-key-pool-cooldown-ms pool)
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

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 1. 默认冷却时长
(check (pi-key-pool-cooldown-ms (pi-key-pool "google")) => 20000)

;; 2. 自定义冷却时长
(check (pi-key-pool-cooldown-ms (pi-key-pool "google" 1500)) => 1500)

(check-report)
