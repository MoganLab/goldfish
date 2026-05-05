(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; call-with-output-string
;; 创建输出字符串端口，将其传给指定过程，然后返回收集的输出字符串。
;;
;; 语法
;; ----
;; (call-with-output-string proc)
;;
;; 参数
;; ----
;; proc - 接受一个端口参数的过程
;;
;; 返回值
;; ------
;; 收集输出的字符串。
;;
;; 说明
;; ----
;; call-with-output-string 是 S7 内置函数，但不在 R7RS 标准中定义。
;;
;; 示例
;; ----
;; (call-with-output-string (lambda (p) (display "hello" p))) => "hello"
;; (call-with-output-string (lambda (p) (display 123 p)))     => "123"
;; (call-with-output-string (lambda (p) (values)))             => ""


(check (call-with-output-string (lambda (p) (display "hello" p))) => "hello")
(check (call-with-output-string (lambda (p) (display 123 p))) => "123")
(check (call-with-output-string (lambda (p) (values))) => "")


(check-report)
