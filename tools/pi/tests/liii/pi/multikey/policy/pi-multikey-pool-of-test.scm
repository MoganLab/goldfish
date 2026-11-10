;; pi-multikey-pool-of
;; 从会话配置中读取绑定的密钥池记录。
;;
;; 语法
;; ----
;; (pi-multikey-pool-of config)
;;
;; 参数
;; ----
;; config : list?
;; 会话配置 alist。
;;
;; 返回值
;; ----
;; (or pi-key-pool? #f)
;; 绑定的密钥池记录，未绑定时返回 #f。

(import (liii check) (liii pi multikey pool) (liii pi multikey policy))

(check-set-mode! 'report-failed)

;; 1. 配置中绑定密钥池时返回该池
(let ((pool (pi-key-pool "google")))
  (check (pi-multikey-pool-of (list (cons 'multikey-pool pool))) => pool)
) ;let

;; 2. 未绑定密钥池时返回 #f
(check (pi-multikey-pool-of '()) => #f)
(check (pi-multikey-pool-of '((api-key . "sk-plain"))) => #f)

(check-report)
