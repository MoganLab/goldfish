(import (liii check)
        (goldfish match))

;; SRFI-262 core subset: extensible pattern matching.

;; ===== 基本模式 =====

;; 数字字面量 + 通配符
(check (match 5 (1 'one) (5 'five) (_ 'other)) => 'five)
(check (match 9 (1 'one) (5 'five) (_ 'other)) => 'other)

;; 字符串/字符/布尔/符号字面量
(check (match "hi" ("hi" 'string-match) (_ 'no)) => 'string-match)
(check (match #\a (#\a 'char-match) (_ 'no)) => 'char-match)
(check (match #t (#t 'bool-match) (_ 'no)) => 'bool-match)

;; quote 模式
(check (match '(1 2) ('(1 2) 'quoted) (_ 'no)) => 'quoted)

;; ===== 列表模式 =====

;; 裸列表 (解构 + 绑定)
(check (match '(1 2 3) ((a b c) (list c b a)) (_ 'no)) => '(3 2 1))
;; 空列表
(check (match '() (() 'empty) (_ 'no)) => 'empty)
;; 长度不符
(check (match '(1 2) ((a b c) 'three) (_ 'no)) => 'no)

;; 显式 list
(check (match '(1 2 3) ((list a b c) (list c b a)) (_ 'no)) => '(3 2 1))

;; cons 模式
(check (match '(1 . 2) ((a . b) (list a b)) (_ 'no)) => '(1 2))
(check (match '(1 2) ((a . b) (list a b)) (_ 'no)) => '(1 (2)))

;; 显式 cons
(check (match '(1 . 2) ((cons a b) (list a b)) (_ 'no)) => '(1 2))

;; ===== vector 模式 =====
(check (match #(1 2 3) ((vector a b c) (list c b a)) (_ 'no)) => '(3 2 1))
(check (match #(1 2) ((vector a b c) 'three) (_ 'no)) => 'no)

;; ===== 谓词 ? =====
(check (match 4 ((? even? x) (list 'even x)) (_ 'odd)) => '(even 4))
(check (match 5 ((? even? x) 'even) (_ 'odd)) => 'odd)
;; ? 带子模式
(check (match 6 ((? even? x) (list 'even x)) (_ 'odd)) => '(even 6))
(check (match 5 ((? number? n) (list 'num n)) (_ 'no)) => '(num 5))

;; ===== and / or / not =====
(check (match 'a ((or 'a 'b) 'ab) (_ 'other)) => 'ab)
(check (match 'c ((or 'a 'b) 'ab) (_ 'other)) => 'other)
(check (match '(1 2) ((and (? list?) (a b)) (list a b)) (_ 'no)) => '(1 2))
(check (match 4 ((not (? even?)) 'odd) (_ 'even)) => 'even)
(check (match 5 ((not (? even?)) 'odd) (_ 'even)) => 'odd)

;; ===== 多 clause 失败 =====
(check (catch #t
         (lambda () (match 1 ((2) 'two) ((3) 'three)))
         (lambda (tag . info) 'no-match))
       => 'no-match)

;; ===== match-lambda =====
(define f1 (match-lambda ((a b) (list b a)) ((a) (list 'single a))))
(check (f1 1 2) => '(2 1))
(check (f1 5) => '(single 5))

;; 多 arity
(define f2 (match-lambda (() 'zero) ((a) (list 'one a)) ((a b) (list 'two a b))))
(check (f2) => 'zero)
(check (f2 1) => '(one 1))
(check (f2 1 2) => '(two 1 2))

;; ===== match-values =====
(check (match-values (values 1 2) ((a b) (list b a))) => '(2 1))

;; ===== match-let / match-let* =====
(check (match-let (((a b) (list 1 2))) (list b a)) => '(2 1))
(check (match-let* (((a b) (list 1 2)) ((c d) (list 3 4))) (list d c b a))
       => '(4 3 2 1))

;; ===== if-match =====
(check (if-match (((a b) (list 1 2))) (list 'm a b) 'nomatch) => '(m 1 2))
(check (if-match (((a b) (list 1))) 'm 'nomatch) => 'nomatch)

;; ===== match-define =====
(match-define (mx my) (list 10 20))
(check (list mx my) => '(10 20))
(match-define ((ma mb) mc) (list (list 1 2) 3))
(check (list ma mb mc) => '(1 2 3))

;; ===== 自定义模式 (define-pattern-syntax) =====
(define-pattern-syntax pair2
  (lambda (stx)
    (let ((d (syntax->datum stx)))
      (datum->syntax stx (list 'cons (cadr d) (caddr d))))))
(check (match (cons 1 2) ((pair2 a b) (list 'p a b)) (_ 'no)) => '(p 1 2))
(check (match (cons 'x 'y) ((pair2 a b) (list a b)) (_ 'no)) => '(x y))

(check-report)
