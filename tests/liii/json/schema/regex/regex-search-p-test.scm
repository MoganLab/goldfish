(import (liii check) (liii json schema regex))

(check-set-mode! 'report-failed)

;; regex-search?
;; 用正则表达式在字符串中搜索匹配。
;;
;; 语法
;; ----
;; (regex-search? pattern value)
;;
;; 参数
;; ----
;; pattern : string
;; 正则表达式，PCRE 方言。
;;
;; value : string
;; 待搜索的字符串。
;;
;; 返回值
;; ----
;; boolean
;; 存在匹配返回 #t，否则返回 #f。
;;
;; 注意
;; ----
;; 该函数是“搜索”而非隐式的整串匹配，
;; 需要整串匹配时请在表达式中自行使用 ^ 与 $。
;; 模式与输入都通过参数传入，不经过 shell。
;;
;; 示例
;; ----
;; (regex-search? "abc" "xxabcxx") => #t
;; (regex-search? "^abc$" "xxabcxx") => #f
;;
;; 错误处理
;; ----
;; json-schema-schema-error 当表达式或输入不是字符串时。
;; json-schema-options-error 当正则后端不可用时。

;; 搜索而非整串匹配
(check-true (regex-search? "abc" "xxabcxx"))
(check-false (regex-search? "^abc$" "xxabcxx"))
(check-true (regex-search? "^abc$" "abc"))

;; Unicode 与量词
(check-true (regex-search? "😀" "x😀x"))
(check-true (regex-search? "a+" "caaat"))

;; 参数不经过 shell，不会被注入
(check-false (regex-search? "a; echo injected" "a"))

;; 非法表达式
(check-catch 'json-schema-schema-error (regex-search? "[" "x"))

;; 类型检查
(check-catch 'json-schema-schema-error (regex-search? 1 "x"))
(check-catch 'json-schema-schema-error (regex-search? "a" 1))

(check-report)
