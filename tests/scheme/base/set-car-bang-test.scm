(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; set-car!基本功能测试
(let ((p (cons 1 2)))
  (set-car! p 100)
  (check p => '(100 . 2))
) ;let
;; set-car!用于列表的首元素修改
(let ((lst (list 'a 'b 'c)))
  (set-car! lst 'x)
  (check lst => '(x b c))
) ;let
;; set-car!测试不同类型的值
(let ((p (cons 'old 'value)))
  (set-car! p "new string")
  (check (car p) => "new string")
  (set-car! p 42)
  (check (car p) => 42)
  (set-car! p #t)
  (check (car p) => #t)
) ;let
;; 使用set-car!修改嵌套结构
(let ((nested (list (list 1 2) (list 3 4))))
  (set-car! (car nested) 'first)
  (check nested => '((first 2) (3 4)))
) ;let
;; set-car!与cons构造器结合测试
(let ((p (cons 'initial 'cdr-value)))
  (check (car p) => 'initial)
  (set-car! p 'modified)
  (check (car p) => 'modified)
  (check (cdr p) => 'cdr-value)
) ;let
;; 多次set-car!调用测试
(let ((lst (list 1 2 3 4 5)))
  (set-car! lst 'first)
  (check lst => '(first 2 3 4 5))
  (set-car! lst 'changed)
  (check lst => '(changed 2 3 4 5))
) ;let
;; 测试set-car!的副作用（变量引用一致性）
(let ((lst1 (list 'a 'b 'c)))
  (let ((lst2 lst1))
    (set-car! lst1 'X)
    (check lst1 => '(X b c))
    (check lst2 => '(X b c))
  ) ;let
) ;let
;; 测试set-car!对不同数据结构的影响
(let ((pair (cons 'head 'tail))
      (alist (list 'a 'b 'c 'd 'e))
     ) ;
  ;; 修改点对结构
  (set-car! pair 'new-head)
  (check pair => '(new-head . tail))
  ;; 修改列表的各个位置（通过访问不同car操作）
  (set-car! alist 'first)
  (check alist => '(first b c d e))
  ;; 验证列表结构保持正确
  (check (length alist) => 5)
  (check (cdr alist) => '(b c d e))
) ;let
;; set-car!错误处理测试
(check-catch 'wrong-type-arg
  (set-car! 123 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (set-car! '() 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (set-car! "string" 'value)
) ;check-catch
(check-catch 'wrong-type-arg
  (set-car! #t 'value)
) ;check-catch
;; 测试参数数量错误
(check-catch 'wrong-number-of-args
  (set-car! (cons 1 2))
) ;check-catch
(check-catch 'wrong-number-of-args
  (set-car! (cons 1 2) 'a 'b)
) ;check-catch
(check-catch 'wrong-number-of-args
  (set-car!)
) ;check-catch
;; 测试复杂对象的set-car!修改
(let ((complex-pair (cons (list 'old-structure 'with-values)
                      'remaining-cdr
                    ) ;cons
      ) ;complex-pair
     ) ;
  (set-car! complex-pair 'simplified)
  (check complex-pair
    =>
    '(simplified . remaining-cdr)
  ) ;check
) ;let
(check-report)