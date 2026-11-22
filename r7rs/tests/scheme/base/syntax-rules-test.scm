;; syntax-rules
;; 创建基于声明式模式匹配的卫生宏转换器。
;;
;;
;; 语法
;; ----
;; (syntax-rules (<literal> ...) <syntax-rule> ...)
;; (syntax-rules <ellipsis> (<literal> ...) <syntax-rule> ...)
;;
;; 参数
;; ----
;; <ellipsis> : identifier（可选）
;; 自定义省略符标识符，默认为 ...。
;;
;; <literal> ... : 标识符列表
;; 匹配时作为字面量比对的标识符列表。
;;
;; <syntax-rule> ... : (<pattern> <template>)
;; 模式与模板规则对。
;;
;; 返回值
;; ----
;; transformer
;; 生成宏转换器过程。

(import (scheme base) (srfi 78))

(check-set-mode! 'report-failed)

;; 用例 4.3.2-1：常量模式匹配与常数模板
(define-syntax const-42
  (syntax-rules ()
    ((const-42) 42)
  ) ;syntax-rules
) ;define-syntax

(check (const-42) => 42)

;; 用例 4.3.2-2：通配符 _ 匹配（忽略中间参数）
(define-syntax ignore-mid
  (syntax-rules ()
    ((ignore-mid a _ c) (list a c))
  ) ;syntax-rules
) ;define-syntax

(check (ignore-mid 1 999 2) => '(1 2))

;; 用例 4.3.2-3：字面量（Literals）匹配
(define-syntax speak
  (syntax-rules (say)
    ((speak say msg) msg)
  ) ;syntax-rules
) ;define-syntax

(check (speak say 'hello) => 'hello)

;; 用例 4.3.2-4：省略号 ... 重复解构与模板展开
(define-syntax my-list
  (syntax-rules ()
    ((my-list x ...) (list x ...))
  ) ;syntax-rules
) ;define-syntax

(check (my-list 1 2 3) => '(1 2 3))
(check (my-list) => '())

;; 用例 4.3.2-5：R7RS 尾部模式匹配 (x ... last)
(define-syntax split-last
  (syntax-rules ()
    ((split-last (x ... last)) (list (list x ...) last))
  ) ;syntax-rules
) ;define-syntax

(check (split-last (1 2 3 4)) => '((1 2 3) 4))

;; 用例 4.3.2-6：向量模式与向量模板 #(a b)
(define-syntax vec-swap
  (syntax-rules ()
    ((vec-swap #(a b)) #(b a))
  ) ;syntax-rules
) ;define-syntax

(check (vec-swap #(1 2)) => #(2 1))

;; 用例 4.3.2-7：自定义省略标识符 (syntax-rules ::: ())
(define-syntax custom-dots
  (syntax-rules
    ::: ()
    ((custom-dots (x :::)) (list x :::))
  ) ;syntax-rules
) ;define-syntax

(check (custom-dots (10 20 30)) => '(10 20 30))

;; 局部语法关键字绑定：let-syntax
(check
  (let-syntax
    ((add1 (syntax-rules () ((add1 x) (+ x 1)))))
    (add1 5)
  ) ;let-syntax
  =>
  6
) ;check

;; 局部递归语法关键字绑定：letrec-syntax
(check
  (letrec-syntax
    ((my-or
       (syntax-rules ()
         ((my-or) #f)
         ((my-or e) e)
         ((my-or e1 e2 ...) (if e1 e1 (my-or e2 ...)))
       ) ;syntax-rules
     ) ;my-or
    ) ;
    (my-or #f #f 42)
  ) ;letrec-syntax
  =>
  42
) ;check

;; 卫生性变量隔离验证：防止调用处变量捕获
(define-syntax hygienic-or
  (syntax-rules ()
    ((hygienic-or e1 e2) (let ((temp e1)) (if temp temp e2)))
  ) ;syntax-rules
) ;define-syntax

(check (let ((temp 99)) (hygienic-or #f temp)) => 99)

;; [1292] 卫生性 quote 隔离验证：防止调用处局部 quote 捕获宏模板中的 'x
(define-syntax hygienic-quote
  (syntax-rules ()
    ((hygienic-quote) 'ok)
    ((hygienic-quote nested) ''ok)
  ) ;syntax-rules
) ;define-syntax

(check (let ((quote 42)) (hygienic-quote)) => 'ok)
(check (equal? (let ((quote 42)) (hygienic-quote nested)) '(quote ok)) => #t)

;; [1290] 宏展开生成的顶层 define 互相引用：前向引用的名字应一致
;; 展开产物 (begin (define (ff x) (gg x)) (define (gg x) (* x x)))
;; 中，ff 体内对 gg 的引用与 gg 的 define 注册名必须是同一绑定。
(define-syntax sr-forward-ref-macro
  (syntax-rules ()
    ((sr-forward-ref-macro fname)
     (begin (define (fname x) (sr-forward-helper x))
            (define (sr-forward-helper x) (* x x))))))
(sr-forward-ref-macro sr-forward-fn)
(check (sr-forward-fn 10) => 100)

;; [1290 #3-5] 宏生宏：内层宏模板中的 'y 替换后必须去标记（strip），
;; 不能把展开器的 gensym 改名标记（如 {x}-15）泄漏为运行期符号
(let ()
  (define-syntax macro-gen-macro-quote
    (syntax-rules () ((macro-gen-macro-quote bar y)
      (define-syntax bar
        (syntax-rules () ((bar x) 'y))))))
  (macro-gen-macro-quote mgmq-inner x)
  (check (mgmq-inner 1) => 'x))

;; 顺序多值绑定宏 let*-values
(check
  (let*-values (((a b) (values 1 2)) ((sum) (values (+ a b))))
    sum
  ) ;let*-values
  =>
  3
) ;check

(check-report)
