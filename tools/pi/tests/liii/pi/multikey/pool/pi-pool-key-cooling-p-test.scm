;; pi-pool-key-cooling?
;; 判断密钥在指定时刻是否仍处于冷却期。
;;
;; 语法
;; ----
;; (pi-pool-key-cooling? pk now-ms)
;;
;; 参数
;; ----
;; pk : pi-pool-key?
;; 密钥记录。
;;
;; now-ms : number?
;; 当前时刻（毫秒）。
;;
;; 返回值
;; ----
;; boolean?
;; 仍在冷却期时返回 #t，否则返回 #f。

(import (liii check)
        (liii pi multikey pool)
) ;import

(check-set-mode! 'report-failed)

;; 1. 未设置冷却时间时不在冷却期
(check (pi-pool-key-cooling? (pi-pool-key "sk-alpha") 1000) => #f)

;; 2. 冷却截止时间晚于当前时刻时处于冷却期
(let ((pk (pi-pool-key "sk-alpha")))
  (pi-pool-key-set-cooldown-until! pk 2000)
  (check (pi-pool-key-cooling? pk 1000) => #t)
  (check (pi-pool-key-cooling? pk 2000) => #f)
  (check (pi-pool-key-cooling? pk 3000) => #f)
) ;let*

(check-report)
