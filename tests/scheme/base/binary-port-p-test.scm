(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; binary-port?
;; 判断端口是否为二进制端口。
;;
;; 语法
;; ----
;; (binary-port? port)
;;
;; 参数
;; ----
;; port : any
;; 待判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果对象是二进制端口则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 在 S7 Scheme 中，所有端口同时被视为二进制端口和文本端口
;; 2. 非端口对象返回 #f
(check (binary-port? (current-input-port)) => #t)
(check (binary-port? (current-output-port)) => #t)
(check (binary-port? '()) => #f)
(check (binary-port? "abc") => #f)
(check (binary-port? 123) => #f)
(check-catch 'wrong-number-of-args (binary-port?))
(check-catch 'wrong-number-of-args (binary-port? 1 2))

(check-report)
