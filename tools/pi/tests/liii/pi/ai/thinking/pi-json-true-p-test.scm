;; pi-json-true?
;; 判断值是否代表 JSON 布尔真值（兼容 #t 与符号 'true）。
;;
;; 语法
;; ----
;; (pi-json-true? v)
;;
;; 参数
;; ----
;; v : any?
;; 待检验的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 v 为 #t 或符号 'true 时返回 #t，否则返回 #f。

(import (liii check) (liii pi ai thinking))

(check-set-mode! 'report-failed)

(check-true (pi-json-true? #t))
(check-true (pi-json-true? 'true))
(check-false (pi-json-true? #f))
(check-false (pi-json-true? 'false))
(check-false (pi-json-true? 'null))
(check-false (pi-json-true? '()))

(check-report)
