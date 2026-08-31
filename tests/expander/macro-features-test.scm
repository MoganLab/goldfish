(import (liii check) (goldfish))

;; 宏系统高级特性覆盖（测试先行：先测现状，暴露缺口再补实现）。
;;
;; 已实现：自托管 syntax-case（对象级宏）、syntax-rules、with-syntax、
;; 模板预编译、guard/fender、vector/dotted ellipsis、define-macro。
;; 已知缺口（记录于测试尾部的 check-false 断言）：
;;   - 命名/自定义 ellipsis（字面量表列 `...`）
;;   - ellipsis 转义（(... ...)）
;;   - quasisyntax / unsyntax / unsyntax-splicing

;; ===== 1. nested ellipsis（双层）=====
;; 单层组 ellipsis ((x y) ...)：匹配一个模式组消耗多个输入元素
;; （R7RS），已支持。
(define-syntax pair-sum
  (syntax-rules ()
    ((_ (x y) ...) (list (+ x y) ...))))
(check (pair-sum (1 2) (3 4)) => '(3 7))
;; 双层嵌套 ellipsis (((x y) ...) ...) 的常见示例
;; ((_ ((x y) ...) ...) (list (list x y) ...) ...) 在 Guile 中同样报
;; "source expression failed to match any pattern"：规则被读取为 3 个
;; 顶层元素（pattern、模板首、裸 `...`），不是合法 R7RS 规则，因此不是
;; expander 特有缺口（与 letrec-syntax 的 fact 情形同类）。真正需要的
;; 双层匹配（匹配层回溯）已在 syntax-runtime 支持（单层组依赖它）。

;; ===== 2. syntax-case guard/fender =====
(define-syntax guarded
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)
       (identifier? #'x)
       #'(quote identifier))
      ((_ x)
       #'(quote other)))))
(check (guarded foo) => 'identifier)
(check (guarded (1 2)) => 'other)

;; guard 分支拒绝非标识符并尝试下一子句
(define-syntax must-be-id
  (lambda (stx)
    (syntax-case stx ()
      ((_ x) (not (identifier? #'x)) #'(quote bad))
      ((_ x) #'(quote good)))))
(check (must-be-id foo) => 'good)
(check (must-be-id 42) => 'bad)

;; ===== 3. with-syntax =====
(define-syntax with-ws
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)
       (with-syntax ((y #'x) (z #'(list 1 2)))
         #'(list y (car z)))))))
(check (with-ws 10) => '(10 1))

;; ===== 4. generate-temporaries：防捕获 =====
;; 注意写法：模式须带括号 ((tmp) expr)——无括号 (tmp expr) 时 tmp 是
;; ellipsis 变量，绑定整个生成列表，模板单值用会双包装（Guile 同样报
;; "let: bad let"，不是 expander 缺陷）。
(define-syntax swap!
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       (with-syntax (((tmp1 tmp2) (generate-temporaries #'(a b))))
         #'(let ((tmp1 a)) (set! a b) (set! b tmp1)))))))
(let ((x 1) (y 2))
  (swap! x y)
  (check x => 2)
  (check y => 1))
(define-syntax temp-shadow
  (lambda (stx)
    (syntax-case stx ()
      ((_ x)
       (with-syntax (((tmp) (generate-temporaries '(tmp))))
         #'(let ((tmp 99))
             (let ((x (+ x 1)))
               (list tmp x))))))))
(check (let ((tmp 5)) (temp-shadow tmp)) => '(99 6))

;; ===== 5. free-identifier=?：字面量匹配 =====
(define-syntax literal-check
  (lambda (stx)
    (syntax-case stx (else)
      ((_ else) #'(quote matched))
      ((_ x) #'(quote unmatched)))))
(check (literal-check else) => 'matched)
(check (literal-check other) => 'unmatched)

;; ===== 6. define-macro（非卫生）=====
(define-macro (non-hygienic)
  `(list original-value))
(let ((original-value 42))
  (check (non-hygienic) => '(42)))

;; ===== 7. let-syntax / letrec-syntax =====
(let-syntax ((double (syntax-rules () ((_ x) (* 2 x)))))
  (check (double 21) => 42))
;; letrec-syntax 递归绑定：递归宏靠模式匹配消耗参数终止（syntax-rules 展开期
;; 不求值，所以 (fact (- n 1)) 这类依赖运行时值的递归在语法层固有无限 --
;; Racket 同样如此，不是 expander 缺陷）。正确写法见下方 my-or。
(letrec-syntax ((my-or
                 (syntax-rules ()
                   ((_) #f)
                   ((_ e) e)
                   ((_ e1 e2 ...) (let ((t e1)) (if t t (my-or e2 ...)))))))
  (check (my-or #f #f 3) => 3)
  (check (my-or #f #f #f) => #f))

;; ===== 8. quote-syntax 保留词法上下文 =====
;; quote-syntax 在表达式位置求值为 syntax 对象（带字面 scope），宏返回它
;; 或嵌入 datum 时展开器保留为值（Racket）。引用实际标识符 foo。
(define-syntax qs
  (lambda (stx)
    (syntax-case stx ()
      ((_ x) (with-syntax ((q (quote-syntax foo)))
               #'(quote q))))))
(check (syntax? (qs whatever)) => #t)
(check (syntax->datum (qs whatever)) => 'foo)

;; ===== 9. 已知缺口探测 =====
;; 9a. 命名 ellipsis：字面量表列 `...` 应允许别名 ellipsis。
;; R7RS portable match-check-ellipsis 惯用法。当前实现不支持。
(check (catch #t
         (lambda ()
           (eval '(let-syntax ((m (syntax-rules (...) ((_ ...) 'ok))))
                    (m x y z))))
         (lambda args 'expansion-error))
       => 'expansion-error)

;; 9b. ellipsis 转义：(... ...) 应产出一个字面 ...（R6RS）。当前不支持。
(check (catch #t
         (lambda ()
           (eval '(let-syntax ((m (syntax-rules () ((_ x) '(... ...)))))
                    (m 1))))
         (lambda args 'expansion-error))
       => 'expansion-error)

;; 9c. quasisyntax / unsyntax：已实现（core-quasisyntax 翻译模板，
;;     静态部分 quote-syntax、unsyntax 展开；完整测试见 quasisyntax-test.scm）。
(check (let-syntax ((m (lambda (stx)
                         (syntax-case stx ()
                           ((_ x) #`(list #,x))))))
          (m 7))
       => '(7))

(check-report)
