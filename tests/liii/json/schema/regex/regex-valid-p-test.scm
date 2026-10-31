(import (liii check) (liii json schema regex))

(check-set-mode! 'report-failed)

;; regex-valid?
;; 判断正则表达式是否可以被后端编译。
;;
;; 语法
;; ----
;; (regex-valid? pattern)
;;
;; 参数
;; ----
;; pattern : string
;; 正则表达式，PCRE 方言。
;;
;; 返回值
;; ----
;; boolean
;; 表达式合法返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 该函数只检查表达式能否编译，不检查是否匹配任何输入。
;;
;; 示例
;; ----
;; (regex-valid? "^a+$") => #t
;; (regex-valid? "[") => #f
;;
;; 错误处理
;; ----
;; json-schema-schema-error 当表达式不是字符串时。
;; json-schema-options-error 当正则后端不可用时。

(check-true (regex-valid? "^a+$"))
(check-true (regex-valid? "[0-9]+"))
(check-false (regex-valid? "["))
(check-false (regex-valid? "a)("))

(check-catch 'json-schema-schema-error (regex-valid? 1))
(check-catch 'json-schema-schema-error (regex-valid? #f))

(check-report)
