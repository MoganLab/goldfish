;; pi-pool-key-cooldown-until
;; 读取密钥记录的冷却截止时间戳。
;;
;; 语法
;; ----
;; (pi-pool-key-cooldown-until pk)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; 返回值
;; ----
;; number?
;; 冷却截止时间（毫秒），初始值为 0。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 1. 新建记录的冷却截止时间为 0
(check (pi-pool-key-cooldown-until (pi-pool-key "sk-alpha")) => 0)

;; 2. 更新冷却截止时间后可读出
(let ((pk (pi-pool-key "sk-alpha")))
  (pi-pool-key-set-cooldown-until! pk 9999)
  (check (pi-pool-key-cooldown-until pk) => 9999)
) ;let

(check-report)
