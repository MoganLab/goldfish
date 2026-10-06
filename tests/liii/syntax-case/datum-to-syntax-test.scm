(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; datum->syntax
;; 将普通 Scheme 原生数据（datum）转换为携带 template-id 词法作用域信息的语法对象。
;;
;; 语法
;; ----
;; (datum->syntax template-id datum)
;;
;; 参数
;; ----
;; template-id : syntax / identifier
;; 模板标识符或语法对象，用于提供词法作用域上下文环境。
;;
;; datum : any
;; 普通的 Scheme 数据结构（如符号、列表、常数等）。
;;
;; 返回值
;; -----
;; syntax
;; 包装了词法上下文环境信息的语法对象。

;; 1. 符号与列表转换
(let ((stx (datum->syntax 'here '(foo 1 2))))
  (check (syntax->datum stx) => '(foo 1 2)))

;; 2. 标量值转换
(let ((stx (datum->syntax 'here 42)))
  (check (syntax->datum stx) => 42))

;; 3. 嵌套结构与向量转换
(let ((stx (datum->syntax 'here '#(a (b c)))))
  (check (syntax->datum stx) => '#(a (b c))))

(check-report)
