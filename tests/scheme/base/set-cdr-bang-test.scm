(import (liii check))
(import (liii list))
(import (scheme base))
(check-set-mode! 'report-failed)
;; set-cdr!
;; 替换序对（pair）的第二个元素（cdr部分）为新值，该操作会直接修改原始序对对象。
;; 
;; 语法
;; ----
;; (set-cdr! pair obj)
;; 
;; 参数
;; ----
;; pair : pair?
;;     要被修改的序对，可以是点对或任何非空列表。必须是一个可以修改的序对对象。
;; 
;; obj : any
;;     要设置的新值，可以是任何类型的对象，包括符号、数字、列表、字符串、布尔值等。
;; 
;; 返回值
;; ------
;; unspecified
;;     根据R7RS规范，返回未指定的值。
;; 
;; 说明
;; ----
;; 1. set-cdr!是一个变异操作，会直接修改原始序对对象的内存内容
;; 2. 替换的是序对的第二个元素（cdr部分）
;; 3. 当应用于列表时，可以修改列表的尾部结构，包括创建循环结构
;; 4. 修改后原始对象的引用仍然指向同一个内存位置
;; 5. 适用于所有序对数据：不论是显式点对 (a . b) 还是非空列表 (a b c ...)
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;;     当第一个参数不是序对（如空列表、数字、字符串等）时抛出错误。
;; wrong-number-of-args
;;     当参数数量不等于2时抛出错误。
;; 
;; 影响范围
;; --------
;; - 该操作会直接影响使用同一引用的所有代码位置
;; - 修改后原始对象的内容立即发生变化
;; - 可以用于构建循环列表结构或扩展列表尾部
;; - 谨慎使用，避免破坏不变量或导致不可预期的副作用
;; 
;; 示例应用场景
;; ------------
;; - 构建循环列表：通过set-cdr!将列表最后一个元素的cdr指向列表自身
;; - 动态修改列表尾部：可以替换整个列表的尾部为一个新列表
;; - 链表操作：在链表数据结构中插入或删除节点
;; set-cdr!
;; 替换序对（pair）的第二个元素（cdr部分）为新值，该操作会直接修改原始序对对象。
;; 
;; 语法
;; ----
;; (set-cdr! pair obj)
;; 
;; 参数
;; ----
;; pair : pair?
;;     要被修改的序对，可以是点对或任何非空列表。必须是一个可以修改的序对对象。
;; 
;; obj : any
;;     要设置的新值，可以是任何类型的对象，包括符号、数字、列表、字符串、布尔值等。
;; 
;; 返回值
;; ------
;; unspecified
;;     根据R7RS规范，返回未指定的值。
;; 
;; 说明
;; ----
;; 1. set-cdr!是一个变异操作，会直接修改原始序对对象的内存内容
;; 2. 替换的是序对的第二个元素（cdr部分）
;; 3. 当应用于列表时，可以修改列表的尾部结构，包括创建循环结构
;; 4. 修改后原始对象的引用仍然指向同一个内存位置
;; 5. 适用于所有序对数据：不论是显式点对 (a . b) 还是非空列表 (a b c ...)
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;;     当第一个参数不是序对（如空列表、数字、字符串等）时抛出错误。
;; wrong-number-of-args
;;     当参数数量不等于2时抛出错误。
;; 
;; 影响范围
;; --------
;; - 该操作会直接影响使用同一引用的所有代码位置
;; - 修改后原始对象的内容立即发生变化
;; - 可以用于构建循环列表结构或扩展列表尾部
;; - 谨慎使用，避免破坏不变量或导致不可预期的副作用
;; 
;; 示例应用场景
;; ------------
;; - 构建循环列表：通过set-cdr!将列表最后一个元素的cdr指向列表自身
;; - 动态修改列表尾部：可以替换整个列表的尾部为一个新列表
;; - 链表操作：在链表数据结构中插入或删除节点
;; set-cdr!
;; 替换序对（pair）的第二个元素（cdr部分）为新值，该操作会直接修改原始序对对象。
;; 
;; 语法
;; ----
;; (set-cdr! pair obj)
;; 
;; 参数
;; ----
;; pair : pair?
;;     要被修改的序对，可以是点对或任何非空列表。必须是一个可以修改的序对对象。
;; 
;; obj : any
;;     要设置的新值，可以是任何类型的对象，包括符号、数字、列表、字符串、布尔值等。
;; 
;; 返回值
;; ------
;; unspecified
;;     根据R7RS规范，返回未指定的值。
;; 
;; 说明
;; ----
;; 1. set-cdr!是一个变异操作，会直接修改原始序对对象的内存内容
;; 2. 替换的是序对的第二个元素（cdr部分）
;; 3. 当应用于列表时，可以修改列表的尾部结构，包括创建循环结构
;; 4. 修改后原始对象的引用仍然指向同一个内存位置
;; 5. 适用于所有序对数据：不论是显式点对 (a . b) 还是非空列表 (a b c ...)
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;;     当第一个参数不是序对（如空列表、数字、字符串等）时抛出错误。
;; wrong-number-of-args
;;     当参数数量不等于2时抛出错误。
;; 
;; 影响范围
;; --------
;; - 该操作会直接影响使用同一引用的所有代码位置
;; - 修改后原始对象的内容立即发生变化
;; - 可以用于构建循环列表结构或扩展列表尾部
;; - 谨慎使用，避免破坏不变量或导致不可预期的副作用
;; 
;; 示例应用场景
;; ------------
;; - 构建循环列表：通过set-cdr!将列表最后一个元素的cdr指向列表自身
;; - 动态修改列表尾部：可以替换整个列表的尾部为一个新列表
;; - 链表操作：在链表数据结构中插入或删除节点
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
