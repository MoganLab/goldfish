;; pi-key-pool-keys
;; 读取密钥池中的密钥记录列表。
;;
;; 语法
;; ----
;; (pi-key-pool-keys pool)
;;
;; 参数
;; ----
;; pool : pi-key-pool?
;; 密钥池记录。
;;
;; 返回值
;; ----
;; list?
;; 密钥记录列表。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 1. 空池返回空列表
(check (pi-key-pool-keys (pi-key-pool "google")) => '())

;; 2. 读取密钥记录列表
(let* ((k1 (pi-pool-key "sk-alpha"))
       (k2 (pi-pool-key "sk-beta" "beta-label"))
       (pool (pi-key-pool "google" 20000 600000 (list k1 k2)))
      ) ;
  (check (length (pi-key-pool-keys pool)) => 2)
  (check (pi-pool-key-key (list-ref (pi-key-pool-keys pool) 0)) => "sk-alpha")
  (check (pi-pool-key-label (list-ref (pi-key-pool-keys pool) 1)) => "beta-label")
) ;let*

(check-report)
