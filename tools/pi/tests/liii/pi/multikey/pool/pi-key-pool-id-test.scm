;; pi-key-pool-id
;; 读取密钥池的标识。
;;
;; 语法
;; ----
;; (pi-key-pool-id pool)
;;
;; 参数
;; ----
;; pool : pi-key-pool?
;; 密钥池记录。
;;
;; 返回值
;; ----
;; string?
;; 密钥池标识字符串。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 读取密钥池标识
(check (pi-key-pool-id (pi-key-pool "google")) => "google")
(check (pi-key-pool-id (pi-key-pool "kimi-coding")) => "kimi-coding")

(check-report)
