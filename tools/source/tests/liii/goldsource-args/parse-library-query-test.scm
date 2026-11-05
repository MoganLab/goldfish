;; parse-library-query
;; 解析形如 "组织/库名" 的查询字符串为 (组织 . 库名) 点对。
;;
;; 语法
;; ----
;; (parse-library-query query)
;;
;; 参数
;; ----
;; query : any
;; 待解析的查询对象。
;;
;; 返回值
;; ------
;; pair? | #f
;; 若为合法两段式库查询字符串则返回 (组织 . 库名) 点对，否则返回 #f。

(import (scheme base) (liii check) (liii goldsource-args))

(check-set-mode! 'report-failed)

;; 测试：正常两段式库查询解析
(check (parse-library-query "liii/string") => '("liii" . "string"))
(check (parse-library-query "scheme/base") => '("scheme" . "base"))
(check (parse-library-query "srfi/srfi-1") => '("srfi" . "srfi-1"))

;; 测试：不含斜杠的查询返回 #f
(check (parse-library-query "string") => #f)

;; 测试：组织名或库名为空字符串返回 #f
(check (parse-library-query "/string") => #f)
(check (parse-library-query "liii/") => #f)
(check (parse-library-query "/") => #f)

;; 测试：多于两段的路径返回 #f
(check (parse-library-query "a/b/c") => #f)

;; 测试：非字符串输入返回 #f
(check (parse-library-query 123) => #f)
(check (parse-library-query 'liii/string) => #f)

(check-report)
