;; pi-pool-key
;; 创建密钥池中的单个密钥记录。
;;
;; 语法
;; ----
;; (pi-pool-key key . opt-label opt-enabled)
;;
;; 参数
;; ----
;; key : string?
;; 密钥字符串。
;;
;; opt-label : string?（可选）
;; 密钥标签，缺省为空字符串。
;;
;; opt-enabled : boolean?（可选）
;; 是否启用，缺省为 #t。
;;
;; 返回值
;; ----
;; pi-pool-key?
;; 新建的密钥记录。

(import (liii check)
        (liii pi multikey pool)
) ;import

(check-set-mode! 'report-failed)

;; 1. 仅提供 key 时标签为空且默认启用
(let ((pk (pi-pool-key "sk-alpha")))
  (check (pi-pool-key-key pk) => "sk-alpha")
  (check (pi-pool-key-label pk) => "")
  (check (pi-pool-key-enabled pk) => #t)
  (check (pi-pool-key-last-used pk) => 0)
  (check (pi-pool-key-cooldown-until pk) => 0)
) ;let*

;; 2. 可选标签与启用状态
(let ((pk (pi-pool-key "sk-beta" "beta-label" #f)))
  (check (pi-pool-key-label pk) => "beta-label")
  (check (pi-pool-key-enabled pk) => #f)
) ;let*

(check-report)
