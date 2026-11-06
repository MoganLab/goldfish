;; combo-keyword?
;; 判断关键字是否为组合器关键字。
;;
;; 语法
;; ----
;; (combo-keyword? keyword)
;;
;; 参数
;; ----
;; keyword : string?
;; 待判断的关键字名称。
;;
;; 返回值
;; ----
;; list | #f
;; 是组合器关键字时返回自该关键字起的剩余关键字表，
;; 否则返回 #f。
;;
;; 说明
;; ----
;; 组合器关键字包括 allOf、anyOf、oneOf、not 与 if。

(import (liii check) (liii json schema keywords-combo))

(check-set-mode! 'report-failed)

(check (if (combo-keyword? "allOf") #t #f) => #t)
(check (if (combo-keyword? "anyOf") #t #f) => #t)
(check (if (combo-keyword? "oneOf") #t #f) => #t)
(check (if (combo-keyword? "not") #t #f) => #t)
(check (if (combo-keyword? "if") #t #f) => #t)

;; 非组合器关键字返回 #f
(check (combo-keyword? "type") => #f)
(check (combo-keyword? "properties") => #f)
(check (combo-keyword? "") => #f)

(check-report)
