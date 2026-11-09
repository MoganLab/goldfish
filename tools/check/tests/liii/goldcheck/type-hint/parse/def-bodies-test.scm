;; def-bodies
;; 提取 define 的函数体表达式列表；case-lambda 聚合各子句 body。
;;
;; 语法
;; ----
;; (def-bodies d)
;;
;; 参数
;; ----
;; d : def 记录（name kind arity line expr）
;;
;; 返回值
;; ------
;; list
;; 函数体表达式列表。

(import (scheme base) (liii check) (liii goldcheck type-hint parse))

(check-set-mode! 'report-failed)

;; 构造 def 记录：(name kind arity line expr)
(define defs
  (list
    ;; (define (f x) body1 body2)：取 (cddr e)
    (list 'f 'lambda 1 1 '(define (f x) (display x) x))
    ;; (define g (case-lambda ...))：value define，体为 case-lambda 形式
    (list 'g 'rest #f 1 '(define g (case-lambda ((a) 1))))
    ;; (define v 42)：无常规体
    (list 'v 'value #f 1 '(define v 42))
  ) ;list
) ;define

;; 1. 普通函数：取 (cddr e)
(check (def-bodies (car defs)) => '((display x) x))

;; 2. (define name (case-lambda ...))：体为 case-lambda 形式本身
(check (def-bodies (cadr defs)) => '((case-lambda ((a) 1))))

;; 3. value 定义无常规体
(check (def-bodies (caddr defs)) => '())

(check-report)
