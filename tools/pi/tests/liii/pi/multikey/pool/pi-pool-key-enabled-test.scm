;; pi-pool-key-enabled
;; 读取密钥记录的启用状态。
;;
;; 语法
;; ----
;; (pi-pool-key-enabled pk)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; 返回值
;; ----
;; boolean?
;; 启用时返回 #t，禁用时返回 #f。

(import (liii check)
        (liii pi multikey pool)
) ;import

(check-set-mode! 'report-failed)

;; 1. 默认启用
(check (pi-pool-key-enabled (pi-pool-key "sk-alpha")) => #t)

;; 2. 显式禁用
(check (pi-pool-key-enabled (pi-pool-key "sk-alpha" "label" #f)) => #f)

(check-report)
