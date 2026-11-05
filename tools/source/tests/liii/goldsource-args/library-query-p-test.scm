;; library-query?
;; 判断给定值是否为合法的库查询字符串（即包含斜杠 "/" 的字符串）。
;;
;; 语法
;; ----
;; (library-query? value)
;;
;; 参数
;; ----
;; value : any
;; 待检查的值。
;;
;; 返回值
;; ------
;; boolean?
;; 若为包含 "/" 的字符串返回 #t，否则返回 #f。

(import (scheme base) (liii check) (liii goldsource-args))

(check-set-mode! 'report-failed)

;; 测试：包含斜杠的字符串为库查询
(check (library-query? "liii/string") => #t)
(check (library-query? "scheme/base") => #t)
(check (library-query? "srfi/srfi-1") => #t)

;; 测试：不包含斜杠的字符串不是库查询
(check (library-query? "string") => #f)
(check (library-query? "") => #f)

;; 测试：非字符串类型返回 #f
(check (library-query? 'liii/string) => #f)
(check (library-query? 123) => #f)
(check (library-query? #f) => #f)

(check-report)
