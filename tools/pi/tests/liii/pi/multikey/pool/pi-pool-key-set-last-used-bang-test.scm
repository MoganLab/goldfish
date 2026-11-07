;; pi-pool-key-set-last-used!
;; 更新密钥记录最近一次被使用的时间戳。
;;
;; 语法
;; ----
;; (pi-pool-key-set-last-used! pk now-ms)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; now-ms : number?
;; 本次使用时间（毫秒）。
;;
;; 返回值
;; ----
;; 不返回
;; 直接更新密钥记录的就近使用时间。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 更新时间戳后可读出
(let ((pk (pi-pool-key "sk-alpha")))
  (pi-pool-key-set-last-used! pk 100)
  (check (pi-pool-key-last-used pk) => 100)
  (pi-pool-key-set-last-used! pk 200)
  (check (pi-pool-key-last-used pk) => 200)
) ;let

(check-report)
