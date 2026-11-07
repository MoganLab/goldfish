;; pi-pool-key-key
;; 读取密钥记录中的密钥字符串。
;;
;; 语法
;; ----
;; (pi-pool-key-key pk)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; 返回值
;; ----
;; string?
;; 密钥字符串。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 读取密钥记录中的密钥字符串
(check (pi-pool-key-key (pi-pool-key "sk-alpha")) => "sk-alpha")
(check (pi-pool-key-key (pi-pool-key "sk-beta" "beta-label")) => "sk-beta")

(check-report)
