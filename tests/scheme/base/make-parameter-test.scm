(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; make-parameter
;; 创建参数对象。
;;
;; 语法
;; ----
;; (make-parameter init [converter])
;;
;; 返回值
;; ----
;; procedure
;; 无参数调用返回当前值；传一个参数则设置新值。

;; 基本用法
(define p (make-parameter 5))
(check (p) => 5)
(p 10)
(check (p) => 10)

;; 带转换器
(define cp (make-parameter 0 (lambda (x) (* x 2))))
(check (cp) => 0)
(cp 3)
(check (cp) => 6)

;; 配合 parameterize 使用
(check (parameterize ((p 42))
         (p))
       => 42)
;; parameterize 结束后恢复原值
(check (p) => 10)

(check-report)
