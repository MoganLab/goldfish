;; pi-pool-key?
;; 判断对象是否为密钥池中的密钥记录。
;;
;; 语法
;; ----
;; (pi-pool-key? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 是密钥记录时返回 #t，否则返回 #f。

(import (liii check) (liii pi multikey pool))

(check-set-mode! 'report-failed)

;; 1. 密钥记录返回 #t
(check (pi-pool-key? (pi-pool-key "sk-alpha")) => #t)

;; 2. 其它对象返回 #f
(check (pi-pool-key? "sk-alpha") => #f)
(check (pi-pool-key? #f) => #f)
(check (pi-pool-key? (pi-key-pool "pool-a")) => #f)

(check-report)
