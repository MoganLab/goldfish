;; pi-pool-key-set-cooldown-until!
;; 更新密钥记录的冷却截止时间戳。
;;
;; 语法
;; ----
;; (pi-pool-key-set-cooldown-until! pk until-ms)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; until-ms : number?
;; 冷却截止时间（毫秒）。
;;
;; 返回值
;; ----
;; 不返回
;; 直接更新密钥记录的冷却截止时间。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 更新冷却截止时间后可读出
(let ((pk (pi-pool-key "sk-alpha")))
  (pi-pool-key-set-cooldown-until! pk 5000)
  (check (pi-pool-key-cooldown-until pk) => 5000)
  (pi-pool-key-set-cooldown-until! pk 8000)
  (check (pi-pool-key-cooldown-until pk) => 8000)
) ;let

(check-report)
