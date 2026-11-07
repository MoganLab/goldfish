;; pi-key-pool?
;; 判断对象是否为密钥池记录。
;;
;; 语法
;; ----
;; (pi-key-pool? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean?
;; 是密钥池记录时返回 #t，否则返回 #f。

(import (liii check)
        (liii pi multikey pool)
) ;import

(check-set-mode! 'report-failed)

;; 1. 密钥池记录返回 #t
(check (pi-key-pool? (pi-key-pool "google")) => #t)

;; 2. 其它对象返回 #f
(check (pi-key-pool? "google") => #f)
(check (pi-key-pool? #f) => #f)
(check (pi-key-pool? (pi-pool-key "sk-alpha")) => #f)

(check-report)
