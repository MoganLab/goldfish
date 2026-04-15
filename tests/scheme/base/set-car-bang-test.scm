(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; set-car!
;; 替换序对（pair）的第一个元素（car部分）为新值，该操作会直接修改原始序对对象。
;; 
;; 语法
;; ----
;; (set-car! pair obj)
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
;; 1. set-car!是一个变异操作，会直接修改原始序对对象的内存内容
;; 2. 索引从0开始，替换的是序对的第一个元素（car部分）
;; 3. 当应用于列表时，修改的是列表的第一个元素
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
;; - 不能用于扩展或缩短列表长度，仅用于替换现有元素
;; - 谨慎使用，避免破坏不变量或导致不可预期的副作用
;; set-car!
;; 替换序对（pair）的第一个元素（car部分）为新值，该操作会直接修改原始序对对象。
;; 
;; 语法
;; ----
;; (set-car! pair obj)
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
;; 1. set-car!是一个变异操作，会直接修改原始序对对象的内存内容
;; 2. 索引从0开始，替换的是序对的第一个元素（car部分）
;; 3. 当应用于列表时，修改的是列表的第一个元素
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
;; - 不能用于扩展或缩短列表长度，仅用于替换现有元素
;; - 谨慎使用，避免破坏不变量或导致不可预期的副作用
;; set-car!
;; 替换序对（pair）的第一个元素（car部分）为新值，该操作会直接修改原始序对对象。
;; 
;; 语法
;; ----
;; (set-car! pair obj)
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
;; 1. set-car!是一个变异操作，会直接修改原始序对对象的内存内容
;; 2. 索引从0开始，替换的是序对的第一个元素（car部分）
;; 3. 当应用于列表时，修改的是列表的第一个元素
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
;; - 不能用于扩展或缩短列表长度，仅用于替换现有元素
;; - 谨慎使用，避免破坏不变量或导致不可预期的副作用
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