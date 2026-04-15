(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; set-cdr!基本功能测试
(let ((p (cons 1 2)))
  (set-cdr! p 100)
  (check p => '(1 . 100))
) ;let
;; set-cdr!用于列表尾元素修改
(let ((lst (list 'a 'b 'c)))
  (set-cdr! (cdr lst) '(x y))
  (check lst => '(a b x y))
) ;let
;; set-cdr!测试不同类型的值
(let ((p (cons 'head 'old)))
  (set-cdr! p "new string")
  (check (cdr p) => "new string")
  (set-cdr! p 42)
  (check (cdr p) => 42)
  (set-cdr! p #t)
  (check (cdr p) => #t)
) ;let
;; 使用set-cdr!修改嵌套结构
(let ((nested (list (list 1 2) (list 3 4))))
  (set-cdr! (car nested) 'tail)
  (check nested => '((1 . tail) (3 4)))
) ;let
;; set-cdr!与cons构造器结合测试
(let ((p (cons 'head 'cdr-value)))
  (check (cdr p) => 'cdr-value)
  (set-cdr! p 'modified)
  (check (cdr p) => 'modified)
  (check (car p) => 'head)
) ;let
;; 使用set-cdr!构建循环结构
(let ((lst (list 'a 'b 'c)))
  (set-cdr! (last-pair lst) lst)
  (check lst => lst)
  (check (list-ref lst 3) => 'a)
  (check (list-ref lst 4) => 'b)
) ;let
;; set-cdr!测试多次调用
(let ((lst (list 1 2 3)))
  (let ((tail (list 'new-tail)))
    (set-cdr! (cdr lst) tail)
    (check lst => '(1 2 new-tail))
  ) ;let
) ;let
;; 测试set-cdr!的副作用（变量引用一致性）
(let ((lst1 (list 'a 'b 'c)))
  (let ((lst2 lst1))
    (set-cdr! lst1 '(x))
    (check lst1 => '(a x))
    (check lst2 => '(a x))
  ) ;let
) ;let
;; 测试set-cdr!对不同数据结构的影响
(let ((pair (cons 'first 'second))
      (lst (list 'a 'b 'c 'd))
     ) ;
  ;; 修改点对结构
  (set-cdr! pair 'new-tail)
  (check pair => '(first . new-tail))
  ;; 将列表尾部替换为单个元素
  (set-cdr! (cdr (cdr lst)) '())
  (check lst => '(a b c))
) ;let
;; 测试set-cdr!与cons结合构建动态结构
(let ((lst (cons 'head 'tail)))
  (set-cdr! lst
    (cons 'second-element 'final)
  ) ;set-cdr!
  (check lst
    =>
    '(head second-element . final)
  ) ;check
) ;let
;; set-cdr!错误处理测试
(check-catch 'wrong-type-arg
  (set-cdr! 123 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (set-cdr! '() 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (set-cdr! "string" 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (set-cdr! #t 'value)
) ;check-catch
;; 测试参数数量错误
(check-catch 'wrong-number-of-args
  (set-cdr! (cons 1 2))
) ;check-catch
(check-catch 'wrong-number-of-args
  (set-cdr! (cons 1 2) 'a 'b)
) ;check-catch
(check-catch 'wrong-number-of-args
  (set-cdr!)
) ;check-catch
;; 测试复杂对象的set-cdr!修改
(let ((complex-pair (cons (list 'head-structure)
                      '(tail-structure remaining)
                    ) ;cons
      ) ;complex-pair
     ) ;
  (set-cdr! complex-pair 'simple-tail)
  (check complex-pair
    =>
    '((head-structure) . simple-tail)
  ) ;check
) ;let
(check (caar '((a . b) . c)) => 'a)
(check-report)