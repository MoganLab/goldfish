;; extract-form-definitions
;; 从单个 Scheme 表单中提取定义的标识符名称列表。
;;
;; 语法
;; ----
;; (extract-form-definitions form)
;;
;; 参数
;; ----
;; form : any
;; 待解析的 Scheme 表达式或表单。
;;
;; 返回值
;; ------
;; list of string
;; 表单中定义的标识符名称字符串列表。

(import (scheme base)
  (liii check)
  (liii goldcheck unit-test source parse)
) ;import

(check-set-mode! 'report-failed)

;; 1. 非 pair 或普通表达式返回空列表
(check (extract-form-definitions 123) => '())
(check (extract-form-definitions 'symbol) => '())
(check (extract-form-definitions '(+ 1 2)) => '())

;; 2. 提取变量与函数定义
(check (extract-form-definitions '(define foo 42)) => '("foo"))
(check (extract-form-definitions '(define (bar x y) (+ x y))) => '("bar"))

;; 3. 提取宏与语法扩展定义
(check
  (extract-form-definitions '(define-syntax my-syn (syntax-rules ())))
  =>
  '("my-syn")
) ;check
(check (extract-form-definitions '(define-macro (my-mac x) x)) => '("my-mac"))

;; 4. 提取记录类型定义
(check
  (extract-form-definitions
    '(define-record-type point (make-point x y) point? (x point-x))
  ) ;extract-form-definitions
  =>
  '("point")
) ;check

;; 5. 递归提取 begin 结构中的嵌套定义
(check (extract-form-definitions '(begin (define a 1) (define (b) 2)))
  =>
  '("b" "a")
) ;check

(check-report)
