(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; call-with-input-string
;; 从字符串创建输入端口，并将其作为参数传给指定过程。
;;
;; 语法
;; ----
;; (call-with-input-string str proc)
;;
;; 参数
;; ----
;; str  - 输入字符串
;; proc - 接受一个端口参数的过程
;;
;; 返回值
;; ------
;; proc 的返回值。
;;
;; 说明
;; ----
;; call-with-input-string 是 S7 内置函数，但不在 R7RS 标准中定义。
;;
;; 示例
;; ----
;; (call-with-input-string "123" (lambda (p) (read p)))         => 123
;; (call-with-input-string "1 2" (lambda (p) (list (read p) (read p)))) => '(1 2)
;; (call-with-input-string "44" (lambda (p) (+ 1 (read p))))   => 45


(check (call-with-input-string "123" (lambda (p) (read p))) => 123)
(check (call-with-input-string "1 2" (lambda (p) (list (read p) (read p)))) => '(1 2))
(check (call-with-input-string "44" (lambda (p) (+ 1 (read p)))) => 45)
(check (call-with-input-string "#t" (lambda (p) (read p))) => #t)


(check-report)
