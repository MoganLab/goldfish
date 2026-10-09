(import (liii check) (liii match))

(check-set-mode! 'report-failed)

;; match
;; 对表达式的求值结果进行模式匹配，并执行首个匹配成功分支的主体。
;;
;; 语法
;; ----
;; (match expr (pattern [(=> failure)] body ...) ...)
;;
;; 参数
;; ----
;; expr : any
;; 待匹配的目标表达式。
;;
;; pattern : pattern
;; 模式规范。支持字面量、通配符 _、变量绑定、点对/列表、向量、
;; 逻辑组合 (and, or, not)、谓词 (? pred [var])、非线性模式、
;; 省略号 (...)、树搜索 (***) 等。
;;
;; failure : identifier (可选)
;; 命名失败续体。调用 (failure) 可放弃当前分支，继续尝试后续分支。
;;
;; body : any
;; 匹配成功后依次执行的表达式序列。
;;
;; 返回值
;; -----
;; any
;; 首个匹配成功分支最后一个 body 表达式的求值结果。
;; 若无可匹配分支，抛出 'match-error 错误。

;; 1. 字面量与原子值匹配
(check (match 'any (_ 'ok)) => 'ok)
(check (match 'ok (x x)) => 'ok)
(check (match 28 (28 'ok)) => 'ok)
(check (match "good" ("bad" 'fail) ("good" 'ok)) => 'ok)
(check (match 'good ('bad 'fail) ('good 'ok)) => 'ok)
(check (match '() (() 'ok)) => 'ok)
(check (match #t (#t 'ok) (#f 'fail)) => 'ok)
(check (match #f (#t 'fail) (#f 'ok)) => 'ok)

;; 2. 列表与点对 (Pair/List)
(check (match '(ok) ((x) x)) => 'ok)
(check (match '(1 2 3) ((a b c) (+ a b c))) => 6)
(check (match '(1 . 2) ((a . b) (+ a b))) => 3)
(check (match '(1 2 . 3) ((a b . c) (list a b c))) => '(1 2 3))

;; 3. 向量 (Vector)
(check (match '#(ok) (#(x) x)) => 'ok)
(check (match '#(1 2 3) (#(a b c) (list c b a))) => '(3 2 1))
(check (match '#(1 2 3) (#(x ...) x)) => '(1 2 3))

;; 4. 逻辑组合 (and, or, not)
(check
  (match 'ok ((and (? symbol?) y) y))
  =>
  'ok
) ;check
(check (match 1 ((or 1 2) 'ok)) => 'ok)
(check (match 2 ((or 1 2) 'ok)) => 'ok)
(check (match 3 ((or 1 2) 'fail) (else 'ok)) => 'ok)
(check
  (match 28 ((not (a . b)) 'ok))
  =>
  'ok
) ;check
(check
  (match '(1 . 2) ((not (a . b)) 'fail) (else 'ok))
  =>
  'ok
) ;check

;; 5. 谓词判断 (?)
(check (match 28 ((? number?) 'ok)) => 'ok)
(check (match 28 ((? number? x) (+ x 1))) => 29)
(check
  (match '(1 2) ((? list? (a b)) (+ a b)))
  =>
  3
) ;check

;; 6. 多次出现同名变量（非线性模式匹配）
(check (match '(ok . ok) ((x . x) x)) => 'ok)
(check (match '(ok . bad) ((x . x) 'bad) (else 'ok)) => 'ok)
(check (match '(1 2 1) ((a b a) (+ a b))) => 3)
(check (match '(1 2 3) ((a b a) 'fail) (else 'ok)) => 'ok)

;; 7. 省略号 (Ellipsis ...)
(check (match '(1 2 3) ((x ...) x)) => '(1 2 3))
(check
  (match '((a . 1) (b . 2) (c . 3)) (((x . y) ...) (list x y)))
  =>
  '((a b c) (1 2 3))
) ;check
(check (match '(1 2 3 4) ((x ... y) (list x y))) => '((1 2 3) 4))
(check (match '(1 2 3 4 5) ((a b x ... y z) (list a b x y z)))
  =>
  '(1 2 (3) 4 5)
) ;check

;; 8. 模式解构 (嵌套列表与向量)
(check
  (match '(1 (2 3)) ((a (b c)) (list a b c)))
  =>
  '(1 2 3)
) ;check
(check (match '#(1 (2 3)) (#(a (b c)) (list a b c))) => '(1 2 3))

;; 9. 树搜索 (***)
(check
  (match '(x (1 2 3)) ((_ *** (a b c)) (list a b c)))
  =>
  '(1 2 3)
) ;check
(check
  (match '(a (b (c d))) ((and (p *** 'd) x) p))
  =>
  '(a b c)
) ;check

;; 10. 失败跳转 (=> failure)
(check
  (match 2 ((? number? n) (=> next) (if (> n 10) 'big (next))) (_ 'small))
  =>
  'small
) ;check

;; 11. 匹配失败抛出 match-error
(check-catch 'match-error (match 1 (2 'ok)))

;; 12. 模式包含 quote / 'quote 字面量匹配 (Issue #1085)
(check (match '(+ 1 2) (('quote x) x) (_ 'ok)) => 'ok)

(check (match '(quote 1) (('quote x) x)) => 1)

(check
  (match '(quote (1 2)) (('quote (a b)) (+ a b)))
  =>
  3
) ;check

(check (match "(+ 1 2)" (("quote" x) x) (_ "ok")) => "ok")

;; 13. 关键字字面量模式匹配 ('let, 'and, 'or, 'not, 'begin, 'lambda, 'if 等)
(check (match '(let 1) (('let x) x)) => 1)
(check (match (list 'let 1) (('let x) x)) => 1)
(check (match '(and 1) (('and x) x)) => 1)
(check (match (list 'and 1) (('and x) x)) => 1)
(check (match '(or 1 2) (('or x y) (+ x y))) => 3)
(check (match '(not #t) (('not x) x)) => #t)
(check (match '(begin 42) (('begin x) x)) => 42)
(check
  (match '(lambda (x) x) (('lambda (x) body) (list x body)))
  =>
  '(x x)
) ;check
(check (match '(if #t 1 2) (('if c t e) (list c t e))) => '(#t 1 2))
(check
  (match '(let ((x 1)) x) (('let bindings body) (list bindings body)))
  =>
  '(((x 1)) x)
) ;check

;; 关键字字面量配合省略号模式
(check
  (match '((let 1) (let 2)) ((('let x) ...) x))
  =>
  '(1 2)
) ;check
(check
  (match '((and 1) (and 2)) ((('and x) ...) x))
  =>
  '(1 2)
) ;check
(check (match '(let 1 2 3) (('let x ...) x)) => '(1 2 3))

;; 关键字字面量不匹配时能正确 fallback
(check (match '(other 1) (('let x) 'let) (('other x) 'other)) => 'other)
(check (match '(other 1) (('and x) 'and) (('other x) 'other)) => 'other)

;; 14. 反引号模式匹配 (Quasiquote & Splicing)
;; 基础反引号与解引用
(check
  (match (list 1 2) (`(,1 ,x) x) (_ 'no))
  =>
  2
) ;check
(check
  (match '(1 "hello") (`(,1 ,s) s))
  =>
  "hello"
) ;check
(check
  (match '(a 2) (`(a ,x) x))
  =>
  2
) ;check
(check
  (match '(a 2) (`(b ,x) x) (_ 'no))
  =>
  'no
) ;check

;; 多重解引用与非线性匹配
(check
  (match (list 1 2 3) (`(,a ,b ,c) (+ a b c)))
  =>
  6
) ;check
(check
  (match (list 1 2 1) (`(,a ,b ,a) (+ a b)))
  =>
  3
) ;check
(check
  (match (list 1 2 3) (`(,a ,b ,a) (+ a b)) (_ 'no))
  =>
  'no
) ;check

;; 拼接解引用 (尾部拼接与全列表拼接)
(check
  (match (list 1 2 3) (`(,1 ,@x) x) (_ 'no))
  =>
  '(2 3)
) ;check
(check
  (match '(1) (`(,1 ,@x) x))
  =>
  '()
) ;check
(check
  (match '(1 2) (`(,1 ,@x) x))
  =>
  '(2)
) ;check
(check
  (match '(1 2 3) (`(,@x) x))
  =>
  '(1 2 3)
) ;check
(check
  (match '() (`(,@x) x))
  =>
  '()
) ;check

;; 非尾部拼接解引用
(check
  (match '(1 2 3 4) (`(,1 ,@x ,4) x))
  =>
  '(2 3)
) ;check
(check
  (match '(1 4) (`(,1 ,@x ,4) x))
  =>
  '()
) ;check
(check
  (match '(1 2 3) (`(,@x ,3) x))
  =>
  '(1 2)
) ;check

;; 点对反引号 (Dotted Quasiquote)
(check
  (match '(1 . 2) (`(,1 . ,x) x))
  =>
  2
) ;check
(check
  (match '(1 2 . 3) (`(,1 ,2 . ,x) x))
  =>
  3
) ;check
(check
  (match '(1 2 3) (`(,1 ,2 . ,@x) x))
  =>
  '(3)
) ;check

;; 嵌套反引号列表
(check
  (match '(1 (2 3)) (`(,1 (,2 ,x)) x))
  =>
  3
) ;check
(check
  (match '(1 (2 3 4)) (`(,1 (,2 ,@x)) x))
  =>
  '(3 4)
) ;check
(check
  (match '(1 (2 3)) (`(,1 ,x) x))
  =>
  '(2 3)
) ;check

;; 显式 quasiquote / unquote / unquote-splicing
(check
  (match '(1 2) (`(,1 ,x) x))
  =>
  2
) ;check
(check
  (match '(1 2 3) (`(,1 ,@x) x))
  =>
  '(2 3)
) ;check

;; 逻辑组合 (or / and) 中的反引号模式
(check
  (match '(1 2) ((or `(,1 ,x) `(,2 ,x)) x))
  =>
  2
) ;check
(check
  (match '(2 3) ((or `(,1 ,x) `(,2 ,x)) x))
  =>
  3
) ;check
(check
  (match '(1 2) ((and `(,1 ,x) (? pair?)) x))
  =>
  2
) ;check

;; 向量反引号字面量
(check (match #(1 2 3) (#(1 2 3) 'ok)) => 'ok)

;; 匹配失败抛出 match-error
(check-catch 'match-error (match '(1 2) ('(1 3) 'ok)))

;; 15. ..1 模式匹配 (至少匹配一次省略号)
;; 15.1 单一元素与多元素匹配
(check (match '(1) ((a ..1) a)) => '(1))
(check (match (list 1 2 3) ((a ..1) a) (_ 'no)) => '(1 2 3))

;; 15.2 空列表与非列表匹配失败
(check (match '() ((a ..1) a) (_ 'ok)) => 'ok)
(check (match 42 ((a ..1) a) (_ 'ok)) => 'ok)
(check (match '(1 . 2) ((a ..1) a) (_ 'improper)) => 'improper)
(check (match '(1 2 . 3) ((a ..1) a) (_ 'improper)) => 'improper)
(check-catch 'match-error (match '() ((a ..1) a)))

;; 15.3 带后续尾部模式 (trailing patterns)
(check (match '(1 2 3 4) ((a ..1 b) (list a b))) => '((1 2 3) 4))
(check (match '(1 2) ((a ..1 b) (list a b))) => '((1) 2))
(check (match '(1) ((a ..1 b) (list a b)) (_ 'fail)) => 'fail)
(check (match '(1 2 3 4) ((a ..1 b c) (list a b c))) => '((1 2) 3 4))
(check (match '(1 2) ((a ..1 b c) (list a b c)) (_ 'fail)) => 'fail)
(check (match '(1 2 . 3) ((a ..1 b) (list a b)) (_ 'improper)) => 'improper)
(check (match '(1 2 3 4 5) ((a b ..1 c d) (list a b c d))) => '(1 (2 3) 4 5))
(check (match '(1) ((a b ..1) (list a b)) (_ 'fail)) => 'fail)
(check (match '(1 2) ((a b ..1) (list a b))) => '(1 (2)))
(check (match '(1 2 3) ((a b ..1) (list a b))) => '(1 (2 3)))

;; 15.4 嵌套模式与谓词解构
(check
  (match '((1 2) (3 4)) (((x y) ..1) (list x y)))
  =>
  '((1 3) (2 4))
) ;check
(check
  (match '((1 2)) (((x y) ..1) (list x y)))
  =>
  '((1) (2))
) ;check
(check
  (match '() (((x y) ..1) (list x y)) (_ 'empty))
  =>
  'empty
) ;check
(check
  (match '(1 2 3) (((? number? n) ..1) n))
  =>
  '(1 2 3)
) ;check
(check
  (match '(1 "two" 3) (((? number? n) ..1) n) (_ 'fail))
  =>
  'fail
) ;check

;; 15.5 向量模式中的 ..1
(check (match '#(1 2 3) (#(a ..1) a)) => '(1 2 3))
(check (match '#(1) (#(a ..1) a)) => '(1))
(check (match '#() (#(a ..1) a) (_ 'empty)) => 'empty)

;; 15.6 字面量常量与 ..1 重复匹配
(check (match '(1 1 1) ((1 ..1) 'ok)) => 'ok)
(check (match '(1 2 1) ((1 ..1) 'ok) (_ 'no)) => 'no)
(check (match '() ((1 ..1) 'ok) (_ 'no)) => 'no)

;; 15.7 衍生宏 match-let / match-lambda
(check
 ((match-lambda ((a ..1) a)) '(1 2 3))
 =>
 '(1 2 3)
) ;check
(check
  (match-let (((a ..1) '(1 2))) a)
  =>
  '(1 2)
) ;check

(check-report)
