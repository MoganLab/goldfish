;; with-syntax
;; 为局部表达式模式匹配绑定模式变量，以便在内部的 syntax 模板中引用。
;;
;; 语法
;; ----
;; (with-syntax ((pattern expr) ...) body ...)
;;
;; 参数
;; ----
;; ((pattern expr) ...) : bindings
;; 语法绑定列表，将各 expr 的求值结果分别按 pattern 模式解构绑定到模式变量。
;;
;; body ... : expressions
;; 在绑定作用域内求值的表达式序列。
;;
;; 返回值
;; -----
;; any
;; 最后一个 body 表达式的求值结果。

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; 1. 局部模式变量解构与计算
(check (with-syntax (((a b) '(10 20)))
         (quasisyntax (sum (unsyntax (+ (syntax a) (syntax b))))))
       => '(sum 30))

;; 2. 多个独立模式绑定
(check (with-syntax ((x 'hello)
                     (y 'world))
         (syntax (x y)))
       => '(hello world))

;; 3. 省略号与列表结构绑定
(check (with-syntax (((item ...) '(a b c)))
         (syntax (list item ...)))
       => '(list a b c))

;; 4. datum->syntax 在 with-syntax 中引入新标识符（issue #1450）
;;    with-syntax 展开体中出现嵌套 syntax-case 时，内层 pattern 变量
;;    不应引用已失效的第一阶段 gensym（%%sc-e1-*），否则报 unbound variable。
(define-syntax bar
  (lambda (x)
    (syntax-case x ()
      ((_ y)
       (with-syntax ((z (datum->syntax (syntax y) 100)))
         (syntax (+ y z)))))))

(check (bar 42) => 142)

;; 5. with-syntax + generate-temporaries 生成临时绑定名（issue #1450）
;;    旧实现下嵌套展开后内层省略号变量引用失效的 gensym，报 unbound variable
(define-syntax swap-args
  (lambda (x)
    (syntax-case x ()
      ((_ (fn e ...))
       (with-syntax (((t ...) (generate-temporaries (syntax (e ...)))))
         (syntax (let ((t e) ...) (fn t ...))))))))

(check (swap-args (+ 1 2 3)) => 6)

;; 6. my-or：with-syntax 单值绑定 + 递归展开
(define-syntax my-or
  (lambda (x)
    (syntax-case x ()
      ((_ e) (syntax e))
      ((_ e1 e2 ...)
       (with-syntax ((t (car (generate-temporaries '(x)))))
         (syntax (let ((t e1)) (if t t (my-or e2 ...)))))))))

(check (my-or #f #f 7) => 7)
(check (my-or 1 2) => 1)

(check-report)
