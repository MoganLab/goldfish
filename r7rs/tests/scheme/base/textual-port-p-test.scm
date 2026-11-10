;; textual-port?
;; 判断端口是否为文本端口。
;;
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
;; ----
;; boolean?
;; 如果对象是文本端口则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. R7RS 6.13.1：端口必须恰为 textual 或 binary 之一
;; 2. 字符串端口、标准输入输出端口是文本端口
;; 3. bytevector 端口是二进制端口，不是文本端口
;; 4. 非端口对象返回 #f

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (textual-port? (current-input-port)) => #t)
(check (textual-port? (current-output-port)) => #t)
(check (textual-port? '()) => #f)
(check (textual-port? "abc") => #f)
(check (textual-port? 123) => #f)
(let ((p (open-input-string "test")))
  (check (textual-port? p) => #t)
  (close-port p)
) ;let
(let ((p (open-input-bytevector (bytevector 1 2))))
  (check (textual-port? p) => #f)
  (close-port p)
) ;let
(check-catch 'wrong-number-of-args (textual-port?))
(check-catch 'wrong-number-of-args (textual-port? 1 2))

(check-report)
