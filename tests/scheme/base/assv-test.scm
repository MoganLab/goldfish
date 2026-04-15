(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; assv 基本功能测试
(check (assv 'a '((a . 1) (b . 2)))
  =>
  '(a . 1)
) ;check
(check (assv 'b '((a . 1) (b . 2)))
  =>
  '(b . 2)
) ;check
(check (assv 'c '((a . 1) (b . 2)))
  =>
  #f
) ;check
;; 边界1：空列表边界
(check (assv 'key '()) => #f)
;; 边界2：单元素列表边界
(check (assv 'k '((k . v))) => '(k . v))
(check (assv 'missing '((k . v))) => #f)
;; 边界3：数值等价性边界测试
(check (assv 42
         '((42 . "int") (42.0 . "float"))
       ) ;assv
  =>
  '(42 . "int")
) ;check
(check (assv 42.0
         '((42 . "int") (42.0 . "float"))
       ) ;assv
  =>
  '(42.0 . "float")
) ;check
(check (assv 3.14
         '((1 . a) (3.14 . b) (5 . c))
       ) ;assv
  =>
  '(3.14 . b)
) ;check
;; 边界4：字符精确匹配边界
(check (assv #\a
         '((#\a . "lowercase") (#\A . "uppercase"))
       ) ;assv
  =>
  '(#\a . "lowercase")
) ;check
(check (assv #\A
         '((#\a . "lowercase") (#\A . "uppercase"))
       ) ;assv
  =>
  '(#\A . "uppercase")
) ;check
(check (assv #\0
         '((#\0 . "zero") (#\1 . "one"))
       ) ;assv
  =>
  '(#\0 . "zero")
) ;check
;; 边界5：布尔值边界测试
(check (assv #t '((#t . true) (#f . false)))
  =>
  '(#t . true)
) ;check
(check (assv #f '((#f . false) (#t . true)))
  =>
  '(#f . false)
) ;check
;; 边界6：重复键处理边界
(check (assv 42
         '((42 . first) (42 . second) (42 . third))
       ) ;assv
  =>
  '(42 . first)
) ;check
(check (assv 'sym
         '((a . 1) (sym . found) (sym . ignored))
       ) ;assv
  =>
  '(sym . found)
) ;check
;; 边界7：嵌套结构作为值边界
(check (assv 'key
         '((key (1 2 3)) (other . "val"))
       ) ;assv
  =>
  '(key (1 2 3))
) ;check
(check (assv 'list
         '((key . "val") (list "item") (other . 123))
       ) ;assv
  =>
  '(list "item")
) ;check
;; 边界8：复杂数据类型边界
(check (assv #t
         '((#t enabled #t #f) (#f disabled))
       ) ;assv
  =>
  '(#t enabled #t #f)
) ;check
(check (assv 99.9
         '((42 . "answer") (99.9 . 100.0) (0 . "zero"))
       ) ;assv
  =>
  '(99.9 . 100.0)
) ;check
;; 数值边界测试
(check (assv 0
         '((0 . zero) (1 . one) (-1 . negative))
       ) ;assv
  =>
  '(0 . zero)
) ;check
(check (assv -1
         '((0 . zero) (-1 . negative) (1 . one))
       ) ;assv
  =>
  '(-1 . negative)
) ;check
(check (assv 2147483647
         '((2147483647 . int-max) (-2147483648 . int-min))
       ) ;assv
  =>
  '(2147483647 . int-max)
) ;check
;; 浮点数边界测试
(check (assv 1.0
         '((1 "integer") (1.0 "float") (1.1 "larger"))
       ) ;assv
  =>
  '(1.0 "float")
) ;check
(check (assv 0.5
         '((0.5 . "exact") (0.51 . "approx"))
       ) ;assv
  =>
  '(0.5 . "exact")
) ;check
;; 字符集合边界测试
(check (assv #\newline
         '((#\newline . "return") (#\tab . "indent"))
       ) ;assv
  =>
  '(#\newline . "return")
) ;check
(check (assv #\space
         '((#\space . "space") (#\a . "char") (#\z . "last"))
       ) ;assv
  =>
  '(#\space . "space")
) ;check
;; 过程对象边界测试 - 过程对象需要使用eq?比较，而不是eqv?
(let ((proc car))
  (check (assv proc
           (list (cons car "first")
             (cons cdr "second")
           ) ;list
         ) ;assv
    =>
    (cons car "first")
  ) ;check
) ;let
;; 特殊符号边界测试
(check (assv :keyword
         '((:keyword . "special") (sym . "normal"))
       ) ;assv
  =>
  '(:keyword . "special")
) ;check
;; 错误处理测试
(check-catch 'wrong-type-arg
  (assv 'key "not-association-list")
) ;check-catch
(check-catch 'wrong-type-arg
  (assv 'key 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (assv 'key #t)
) ;check-catch
;; 参数数量错误测试
(check-catch 'wrong-number-of-args
  (assv)
) ;check-catch
(check-catch 'wrong-number-of-args
  (assv 'key)
) ;check-catch
(check-catch 'wrong-number-of-args
  (assv 'key 'list "extra")
) ;check-catch
;; 非配对结构边界测试
(check (assv 'a '((a) (b) (c))) => '(a))
(check (assv 'b '((a) (b) (c))) => '(b))
;; 嵌套关联列表边界
(check (assv 42 '((42 a b c) (foo . bar)))
  =>
  '(42 a b c)
) ;check
(check (assv 'x '((x 1 2 3) (y 4 5 6)))
  =>
  '(x 1 2 3)
) ;check
(check-report)