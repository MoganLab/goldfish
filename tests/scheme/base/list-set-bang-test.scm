(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list-set! 基本功能测试
(let ((lst (list 'a 'b 'c)))
  (list-set! lst 1 'x)
  (check lst => '(a x c))
) ;let
;; 边界值测试：索引极端值
(let ((lst (list 'single)))
  (list-set! lst 0 'modified)
  (check lst => '(modified))
) ;let
(let ((lst (list 'first 'middle 'last)))
  (list-set! lst 0 'start)
  (list-set! lst 2 'end)
  (check lst => '(start middle end))
) ;let
(let ((lst (list 'a 'b 'c 'd)))
  (list-set! lst 3 'last)
  (check lst => '(a b c last))
) ;let
;; 空列表面界测试
(let ((lst (list)))
  (check-catch 'wrong-type-arg
    (list-set! lst 0 'value)
  ) ;check-catch
) ;let
;; 各种数据类型测试
(let ((lst (list 1 "text" #t 'symbol)))
  (list-set! lst 0 42)
  (list-set! lst 1 "modified")
  (list-set! lst 2 #f)
  (list-set! lst 3 'changed)
  (check lst
    =>
    '(42 "modified" #f changed)
  ) ;check
) ;let
;; 字符数据类型测试
(let ((lst (list #\a #\b #\c #\d)))
  (list-set! lst 0 #\A)
  (list-set! lst 3 #\D)
  (check lst => '(#\A #\b #\c #\D))
) ;let
;; 字符串数据类型测试
(let ((lst (list "hello" "world" "test")))
  (list-set! lst 1 "modified")
  (check lst
    =>
    '("hello" "modified" "test")
  ) ;check
) ;let
;; 布尔数据类型测试
(let ((lst (list #t #f #t)))
  (list-set! lst 1 #t)
  (check lst => '(#t #t #t))
) ;let
;; 符号数据类型测试
(let ((lst (list 'define 'lambda 'if 'cond)))
  (list-set! lst 2 'when)
  (check lst
    =>
    '(define lambda when cond)
  ) ;check
) ;let
;; 过程数据类型测试
(let ((lst (list car cdr cons)))
  (list-set! lst 0 list)
  (check (procedure? (list-ref lst 0))
    =>
    #t
  ) ;check
) ;let
;; 嵌套子列表结构测试
(let ((lst (list '(a b) '(c d) '(e f))))
  (list-set! lst 1 '(x y z))
  (check lst => '((a b) (x y z) (e f)))
) ;let
;; 嵌套结构替换测试
(let ((lst (list (list 'a) (list 'b) (list 'c))
      ) ;lst
     ) ;
  (list-set! lst 1 (list 'x 'y))
  (check lst => '((a) (x y) (c)))
) ;let
;; 深度嵌套结构测试
(let ((lst (list (list (list 1))
             (list 2)
             (list (list 3))
           ) ;list
      ) ;lst
     ) ;
  (list-set! lst 1 (list 'new 'structure))
  (check lst
    =>
    '(((1)) (new structure) ((3)))
  ) ;check
) ;let
;; 向量和字节向量元素测试
(let ((lst (list #(1 2 3) #u(255 128))))
  (list-set! lst 0 #(4 5 6))
  (list-set! lst 1 #u(100 200))
  (check lst => '(#(4 5 6) #u(100 200)))
) ;let
;; Unicode字符串元素测试
(let ((lst (list "中文" "测试" "字符串")
      ) ;lst
     ) ;
  (list-set! lst 1 "修改")
  (check lst
    =>
    '("中文" "修改" "字符串")
  ) ;check
) ;let
;; 构造器函数列表测试
(let ((lst (make-list 4 'placeholder)))
  (list-set! lst 1 'second)
  (list-set! lst 3 'last)
  (check lst
    =>
    '(placeholder second placeholder last)
  ) ;check
) ;let
;; 长列表性能测试
(let ((lst (list 1
             2
             3
             4
             5
             6
             7
             8
             9
             10
             11
             12
             13
             14
             15
             16
             17
             18
             19
             20
           ) ;list
      ) ;lst
     ) ;
  (list-set! lst 5 'five)
  (list-set! lst 15 'fifteen)
  (check lst
    =>
    '(1 2 3 4 5 five 7 8 9 10 11 12 13 14 15 fifteen 17 18 19 20)
  ) ;check
) ;let
;; 空字符串和空列表元素测试
(let ((lst (list "" (list) "abc")))
  (list-set! lst 1 (list 'x 'y 'z))
  (check lst => '("" (x y z) "abc"))
) ;let
;; 数值和浮点数元素测试
(let ((lst (list 1 2.5 3 4.0)))
  (list-set! lst 2 99)
  (list-set! lst 3 100.0)
  (check lst => '(1 2.5 99 100.0))
) ;let
;; 函数替换测试
(let ((lst (list #t #f #t #f)))
  (list-set! lst 2 42)
  (list-set! lst 3 "hello")
  (check lst => '(#t #f 42 "hello"))
) ;let
;; 错误参数类型测试
(check-catch 'wrong-type-arg
  (list-set! 123 0 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-set! "string" 1 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-set! #t 0 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (list-set! 'symbol 1 'value)
) ;check-catch
;; 索引越界测试
(let ((lst (list 'a 'b 'c)))
  (check-catch 'out-of-range
    (list-set! lst -1 'value)
  ) ;check-catch
  (check-catch 'out-of-range
    (list-set! lst 3 'value)
  ) ;check-catch
  (check-catch 'out-of-range
    (list-set! lst 4 'value)
  ) ;check-catch
) ;let
(let ((lst (list 'single)))
  (check-catch 'out-of-range
    (list-set! lst 1 'value)
  ) ;check-catch
  (check-catch 'out-of-range
    (list-set! lst -1 'value)
  ) ;check-catch
) ;let
;; 参数数量错误测试
(check-catch 'wrong-number-of-args
  (list-set!)
) ;check-catch
(check-catch 'wrong-number-of-args
  (list-set! '(a b c))
) ;check-catch
(check-catch 'wrong-number-of-args
  (list-set! '(a b c) 1)
) ;check-catch
(check-catch 'wrong-number-of-args
  (list-set! '(a b c) 1 'x 'y)
) ;check-catch
;; 大整数索引边界测试
(let ((lst '(a b c d e f g h i j)))
  (check-catch 'out-of-range
    (list-set! lst 11 'value)
  ) ;check-catch
) ;let
;; 验证突变影响（共享引用测试）
(let ((original (list 'a 'b 'c 'd 'e)))
  (let ((shared original))
    (list-set! shared 2 'modified)
    (check original => '(a b modified d e))
    (check (eq? original shared) => #t)
  ) ;let
) ;let
;; 确保修改正确性验证（修改为list-ref检查）
(let ((lst (list 'alpha 'beta 'gamma)))
  (let ((val-before (list-ref lst 1)))
    (list-set! lst 1 'changed)
    (let ((val-after (list-ref lst 1)))
      (check val-after => 'changed)
    ) ;let
  ) ;let
) ;let
;; 被修改引用关系保持测试
(let ((lst (cons 10 (cons 20 (cons 30 '()))))
     ) ;
  (list-set! lst 1 200)
  (list-set! lst 2 300)
  (check lst => '(10 200 300))
) ;let
(check-report)