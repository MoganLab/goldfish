;; pi-skill?
;; 判断对象是否为 <pi-skill> 技能记录。
;;
;; 语法
;; ----
;; (pi-skill? obj) -> boolean
;;
;; 参数
;; ----
;; obj : any
;; 待判断对象。
;;
;; 返回值
;; ------
;; boolean
;; 是技能记录时为 #t。

(import (scheme base) (liii check) (liii pi code core skills types))

(check-set-mode! 'report-failed)

(check-true (pi-skill? (%make-pi-skill "a" "d" "/f" "/b" #f)))
(check-false (pi-skill? "not-a-skill"))
(check-false (pi-skill? '()))
(check-false (pi-skill? #f))

(check-report)
