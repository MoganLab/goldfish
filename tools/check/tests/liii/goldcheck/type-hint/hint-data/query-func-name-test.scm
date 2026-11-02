;; query-func-name
;; 根据类型提示库名符号列表推导约定的查询函数名符号。
;;
;; 语法
;; ----
;; (query-func-name lib-name-syms)
;;
;; 参数
;; ----
;; lib-name-syms : list of symbol   库名各段符号列表（如 '(hint scheme base)）
;;
;; 返回值
;; ------
;; symbol
;; 查询函数名符号，如 'hint-scheme/base。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 以 hint 开头的库名去掉前缀并用 / 拼接
(check (query-func-name '(hint scheme base)) => 'hint-scheme/base)
(check (query-func-name '(hint liii string)) => 'hint-liii/string)

;; 2. 非 hint 开头直接拼接
(check (query-func-name '(scheme file)) => 'hint-scheme/file)

(check-report)
