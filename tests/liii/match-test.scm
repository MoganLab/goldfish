(import (liii check) (liii match))

(check-set-mode! 'report-failed)

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

;; 11. match 派生形式 (match-lambda, match-lambda*, match-let, match-let*, match-letrec)
(check
 ((match-lambda ((x y) (+ x y))) '(10 20))
 =>
 30
) ;check
(check
 ((match-lambda* ((x y) (+ x y))) 10 20)
 =>
 30
) ;check
(check
  (match-let (((x y) '(1 2)) ((z) '(3))) (+ x y z))
  =>
  6
) ;check
(check
  (match-let*
   ((x 1) ((y) (list (+ x 1))))
   (+ x y)
  ) ;match-let*
  =>
  3
) ;check
(check
  (match-let loop
   (((x . rest) '(1 2 3 4)) (sum 0))
   (if (null? rest) (+ sum x) (loop rest (+ sum x)))
  ) ;match-let
  =>
  10
) ;check
(check
  (match-letrec
   (((even? odd?)
     (list
       (lambda (n) (if (zero? n) #t (odd? (- n 1))))
       (lambda (n) (if (zero? n) #f (even? (- n 1))))
     ) ;list
    ) ;
   ) ;
   (even? 10)
  ) ;match-letrec
  =>
  #t
) ;check

;; 12. 匹配失败时抛出 match-error
(check-catch 'match-error (match 1 (2 'ok)))
(check-catch 'match-error ((match-lambda (('a) 'ok)) '(b)))

(check-report)
