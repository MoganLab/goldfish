;; path?
;; 判断对象是否为路径记录。
;;
;; 语法
;; ----
;; (path? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 是路径记录返回 #t，否则 #f。

(import (liii check) (liii path))

(check-set-mode! 'report-failed)

(check (path? (path "/a/b")) => #t)
(check (path? (path "a")) => #t)
(check (path? "/a/b") => #f)
(check (path? 42) => #f)
(check (path? #f) => #f)

(check-report)
