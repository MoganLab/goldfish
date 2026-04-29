(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; textual-port?
;; 判断端口是否为文本端口。
;;
;; 语法
;; ----
;; (textual-port? port)
;;
;; 参数
;; ----
;; port : any
;; 待判断的对象。
;;
;; 返回值
;; ------
;; boolean?
;; 如果对象是文本端口则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 在 S7 Scheme 中，所有端口同时被视为二进制端口和文本端口
;; 2. 非端口对象返回 #f
(check (textual-port? (current-input-port)) => #t)
(check (textual-port? (current-output-port)) => #t)
(check (textual-port? '()) => #f)
(check (textual-port? "abc") => #f)
(check (textual-port? 123) => #f)
(check-catch 'wrong-number-of-args (textual-port?))
(check-catch 'wrong-number-of-args (textual-port? 1 2))

(check-report)
