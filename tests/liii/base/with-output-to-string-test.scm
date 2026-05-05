(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; with-output-to-string
;; 将过程体内的输出收集到字符串中返回。
;;
;; 语法
;; ----
;; (with-output-to-string thunk)
;;
;; 参数
;; ----
;; thunk - 无参数过程
;;
;; 返回值
;; ------
;; 收集输出的字符串。
;;
;; 说明
;; ----
;; with-output-to-string 是 S7 内置函数，但不在 R7RS 标准中定义。
;;
;; 示例
;; ----
;; (with-output-to-string (lambda () (display "hello"))) => "hello"
;; (with-output-to-string (lambda () (display 123)))     => "123"
;; (with-output-to-string (lambda () (values)))           => ""


(check (with-output-to-string (lambda () (display "hello"))) => "hello")
(check (with-output-to-string (lambda () (display 123))) => "123")
(check (with-output-to-string (lambda () (values))) => "")


(check-report)
