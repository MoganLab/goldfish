;; binary-port?
;; 判断端口是否为二进制端口。
;;
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
;; ----
;; boolean?
;; 如果对象是二进制端口则返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. R7RS 6.13.1：端口必须恰为 textual 或 binary 之一
;; 2. 由 open-*-bytevector / open-binary-*-file 打开的端口是二进制端口
;; 3. 标准输入输出端口是文本端口
;; 4. 非端口对象返回 #f

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (binary-port? (current-input-port)) => #f)
(check (binary-port? (current-output-port)) => #f)
(check (binary-port? '()) => #f)
(check (binary-port? "abc") => #f)
(check (binary-port? 123) => #f)
;; bytevector 端口是二进制端口
(let ((p (open-input-bytevector (bytevector 1 2 3))))
  (check (binary-port? p) => #t)
  (close-port p)
) ;let
(let ((p (open-output-bytevector)))
  (check (binary-port? p) => #t)
  (close-port p)
) ;let
;; 字符串端口是文本端口
(let ((p (open-input-string "test")))
  (check (binary-port? p) => #f)
  (close-port p)
) ;let
(check-catch 'wrong-number-of-args (binary-port?))
(check-catch 'wrong-number-of-args (binary-port? 1 2))

(check-report)
