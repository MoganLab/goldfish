;; json-schema-format-checker?
;; 判断对象是否为格式检查器。
;;
;; 语法
;; ----
;; (json-schema-format-checker? value)
;;
;; 参数
;; ----
;; value : any?
;; 待检查的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是过程返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 格式检查器是接受字符串并返回布尔值的过程。
;;
;; 错误处理
;; ----
;; 无。

(import (liii check) (liii json schema format))

(check-set-mode! 'report-failed)

;; 过程是合法的格式检查器。

(check-true (json-schema-format-checker? (lambda (value) #t)))
(check-true (json-schema-format-checker? string?))

;; 其它对象不是格式检查器。

(check-false (json-schema-format-checker? "email"))
(check-false (json-schema-format-checker? #t))
(check-false (json-schema-format-checker? '()))

(check-report)
