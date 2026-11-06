;; config-parser?
;; 判断值是否为 <config-parser> 类型。
;;
;; 语法
;; ----
;; (config-parser? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检测的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 obj 为配置解析器时返回 #t，否则返回 #f。

(import (liii check) (liii config-parser) (scheme base))

(check-set-mode! 'report-failed)

(check (config-parser? (make-config-parser)) => #t)

(check (config-parser? 42) => #f)
(check (config-parser? "string") => #f)
(check (config-parser? '()) => #f)
(check (config-parser? #f) => #f)

(check-report)
