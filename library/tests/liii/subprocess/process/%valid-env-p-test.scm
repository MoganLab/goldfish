;; %valid-env?
;; 校验环境变量列表的结构与键值类型是否合法。
;;
;; 语法
;; ----
;; (%valid-env? env)
;;
;; 参数
;; ----
;; env : any
;; 待校验的环境变量对象。
;;
;; 返回值
;; ------
;; boolean
;; 合法时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; - 若 env 为 #f，视为空配置放行，返回 #t。
;; - 合法的环境变量必须是规范列表（proper list）。
;; - 列表中的每一个元素必须是 pair，且 car 和 cdr 均为字符串。

(import (liii check) (liii subprocess process))

(check-set-mode! 'report-failed)

;; #f 允许通过
(check (%valid-env? #f) => #t)

;; 空列表允许通过
(check (%valid-env? '()) => #t)

;; 合法的键值对列表
(check (%valid-env? '(("PATH" . "/bin") ("USER" . "guest"))) => #t)

;; 非列表结构返回 #f
(check (%valid-env? 123) => #f)
(check (%valid-env? "PATH=/bin") => #f)
(check (%valid-env? 'symbol) => #f)

;; 非规范列表（improper list）拦截返回 #f
(check (%valid-env? '(("A" . "1") "B" . "2")) => #f)

;; 列表元素非 pair
(check (%valid-env? '("PATH=/bin")) => #f)
(check (%valid-env? '(123)) => #f)

;; pair 的 car 或 cdr 非 string
(check (%valid-env? '((PATH . "/bin"))) => #f)
(check (%valid-env? '(("PORT" . 8080))) => #f)
(check (%valid-env? '((1 . 2))) => #f)

(check-report)
