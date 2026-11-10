;; pi-key-pool
;; 创建密钥池记录。
;;
;; 语法
;; ----
;; (pi-key-pool id . opt-cooldown-ms opt-invalid-cooldown-ms opt-keys)
;;
;; 参数
;; ----
;; id : string?
;; 密钥池标识（通常是 provider 名）。
;;
;; opt-cooldown-ms : integer?（可选）
;; 普通限流冷却时长（毫秒），缺省为 20000。
;;
;; opt-invalid-cooldown-ms : integer?（可选）
;; 密钥失效冷却时长（毫秒），缺省为 600000。
;;
;; opt-keys : list?（可选）
;; 密钥记录列表，缺省为空列表。
;;
;; 返回值
;; ----
;; pi-key-pool?
;; 新建的密钥池记录。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 1. 仅提供 id 时使用默认冷却时长
(let ((pool (pi-key-pool "google")))
  (check (pi-key-pool-id pool) => "google")
  (check (pi-key-pool-cooldown-ms pool) => 20000)
  (check (pi-key-pool-invalid-cooldown-ms pool) => 600000)
  (check (pi-key-pool-keys pool) => '())
) ;let

;; 2. 可选冷却时长与密钥列表
(let* ((k (pi-pool-key "sk-alpha")) (pool (pi-key-pool "google" 1000 2000 (list k))))
  (check (pi-key-pool-cooldown-ms pool) => 1000)
  (check (pi-key-pool-invalid-cooldown-ms pool) => 2000)
  (check (length (pi-key-pool-keys pool)) => 1)
  (check (pi-pool-key-key (car (pi-key-pool-keys pool))) => "sk-alpha")
) ;let*

(check-report)
