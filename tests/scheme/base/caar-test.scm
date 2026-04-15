(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; 基础测试：显式点对结构
(check (caar '((a . b) . c)) => 'a)
(check (caar '((1 . 2) . 3)) => 1)
(check (caar '((#t . #f) . nil)) => #t)
;; 基础测试：列表结构
(check (caar '((a b c) d e)) => 'a)
(check (caar '((1 2 3) 4 5)) => 1)
(check (caar '((#t #f) x y z)) => #t)
;; 嵌套列表结构
(check (caar '(((a b) c) d e))
  =>
  '(a b)
) ;check
(check (caar '(((() a) b) c))
  =>
  '(() a)
) ;check
(check (caar '(((1 2) 3) 4)) => '(1 2))
;; 混合结构测试
(check (caar '(("hello" . 123) . "world"))
  =>
  "hello"
) ;check
(check (caar '((42 . "forty-two") . 99))
  =>
  42
) ;check
(check (caar '((#\a . #\b) . nil))
  =>
  #\a
) ;check
;; 构造器创建的结构
(check (caar (cons (cons 1 2) (cons 3 4)))
  =>
  1
) ;check
(check (caar (cons (cons 'x 'y) (cons 'z 'w)))
  =>
  'x
) ;check
(check (caar (cons (list 1 2 3) (list 4 5 6)))
  =>
  1
) ;check
;; 复杂嵌套构造
(let ((nested (cons (cons (cons 1 2) (cons 3 4))
                (cons 5 6)
              ) ;cons
      ) ;nested
     ) ;
  (check (caar nested) => (cons 1 2))
) ;let
;; 涉及空列表的测试
(check-catch 'wrong-type-arg
  (caar '(() . c))
) ;check-catch
(check-catch 'wrong-type-arg
  (caar '(()))
) ;check-catch
;; 非法参数类型错误
(check-catch 'wrong-type-arg (caar 'a))
(check-catch 'wrong-type-arg (caar 123))
(check-catch 'wrong-type-arg
  (caar "hello")
) ;check-catch
(check-catch 'wrong-type-arg (caar #f))
(check-catch 'wrong-type-arg (caar '()))
(check-catch 'wrong-type-arg
  (caar '(a b . c))
) ;check-catch
;; 返回不同类型测试
(check (caar '(("string" "another") 42))
  =>
  "string"
) ;check
(check (caar '((123 456) 789)) => 123)
(check (caar '(((1 2 3)) 4 5 6))
  =>
  (list 1 2 3)
) ;check
(check (caar '((#f nil "test") x y z))
  =>
  #f
) ;check
;; edge cases for nested structure
(check (caar '((((a))))) => '((a)))
(check (caar '((((1 2))) 3 4 5))
  =>
  '((1 2))
) ;check
(check-report)