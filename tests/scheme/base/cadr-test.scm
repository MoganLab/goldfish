(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; cadr
;; cadr 是 Scheme 内置函数，用于获取序对的第二个元素的第一个分量。该函数是 R7RS 标准的基本列表操作函数之一，等价于 (car (cdr pair))。
;; 
;; 语法
;; ----
;; (cadr pair)
;; 
;; 参数
;; ----
;; pair : pair?
;; 可以是序对（即非空列表或显式点对），不能是空列表或其他对象。
;; 
;; 返回值
;; ------
;; 任意类型
;; 返回序对的第二个元素的第一个分量。根据不同的序对内容，返回类型可以是
;; 符号、数字、列表、点对、布尔值等任何对象。
;; 
;; 说明
;; ----
;; 1. cadr 是 pair? 谓词的基本操作之一，与 car、cdr 和 caar 配合使用处理序对数据
;; 2. 当应用于列表时，返回列表的第二个元素
;; 3. 适用于所有序对数据：不论是点对 (a b . c) 还是非空列表 (a b c ...)
;; 4. 等价于 (car (cdr pair))，执行顺序从右到左解析
;; 5. 是 R7RS 标准 car/cdr 组合函数的之一
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是序对（如空列表 '()、数字、字符串等）或序对的cadr不是序对时抛出错误。
;; 
;; 边界条件
;; --------
;; - 当序对长度为1时，cadr 可能不是序对，需要抛出异常
;; - 单元素列表因没有cadr会引发错误
;; - 正确支持两元素及以上列表的访问
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(1) 恒定时间完成操作
;; - 空间复杂度：O(1) 不消耗额外栈空间
;; - 内存分配：直接访问现有序对结构，无新对象创建
;; 
;; 数据类型兼容性
;; -------------
;; - 支持所有类型的列表和序对结构
;; - 各元素可以是任意 Scheme 对象
;; - 正确处理嵌套列表和复杂结构
;; cadr
;; cadr 是 Scheme 内置函数，用于获取序对的第二个元素的第一个分量。该函数是 R7RS 标准的基本列表操作函数之一，等价于 (car (cdr pair))。
;; 
;; 语法
;; ----
;; (cadr pair)
;; 
;; 参数
;; ----
;; pair : pair?
;; 可以是序对（即非空列表或显式点对），不能是空列表或其他对象。
;; 
;; 返回值
;; ------
;; 任意类型
;; 返回序对的第二个元素的第一个分量。根据不同的序对内容，返回类型可以是
;; 符号、数字、列表、点对、布尔值等任何对象。
;; 
;; 说明
;; ----
;; 1. cadr 是 pair? 谓词的基本操作之一，与 car、cdr 和 caar 配合使用处理序对数据
;; 2. 当应用于列表时，返回列表的第二个元素
;; 3. 适用于所有序对数据：不论是点对 (a b . c) 还是非空列表 (a b c ...)
;; 4. 等价于 (car (cdr pair))，执行顺序从右到左解析
;; 5. 是 R7RS 标准 car/cdr 组合函数的之一
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是序对（如空列表 '()、数字、字符串等）或序对的cadr不是序对时抛出错误。
;; 
;; 边界条件
;; --------
;; - 当序对长度为1时，cadr 可能不是序对，需要抛出异常
;; - 单元素列表因没有cadr会引发错误
;; - 正确支持两元素及以上列表的访问
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(1) 恒定时间完成操作
;; - 空间复杂度：O(1) 不消耗额外栈空间
;; - 内存分配：直接访问现有序对结构，无新对象创建
;; 
;; 数据类型兼容性
;; -------------
;; - 支持所有类型的列表和序对结构
;; - 各元素可以是任意 Scheme 对象
;; - 正确处理嵌套列表和复杂结构
;; cadr
;; cadr 是 Scheme 内置函数，用于获取序对的第二个元素的第一个分量。该函数是 R7RS 标准的基本列表操作函数之一，等价于 (car (cdr pair))。
;; 
;; 语法
;; ----
;; (cadr pair)
;; 
;; 参数
;; ----
;; pair : pair?
;; 可以是序对（即非空列表或显式点对），不能是空列表或其他对象。
;; 
;; 返回值
;; ------
;; 任意类型
;; 返回序对的第二个元素的第一个分量。根据不同的序对内容，返回类型可以是
;; 符号、数字、列表、点对、布尔值等任何对象。
;; 
;; 说明
;; ----
;; 1. cadr 是 pair? 谓词的基本操作之一，与 car、cdr 和 caar 配合使用处理序对数据
;; 2. 当应用于列表时，返回列表的第二个元素
;; 3. 适用于所有序对数据：不论是点对 (a b . c) 还是非空列表 (a b c ...)
;; 4. 等价于 (car (cdr pair))，执行顺序从右到左解析
;; 5. 是 R7RS 标准 car/cdr 组合函数的之一
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;; 当参数不是序对（如空列表 '()、数字、字符串等）或序对的cadr不是序对时抛出错误。
;; 
;; 边界条件
;; --------
;; - 当序对长度为1时，cadr 可能不是序对，需要抛出异常
;; - 单元素列表因没有cadr会引发错误
;; - 正确支持两元素及以上列表的访问
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(1) 恒定时间完成操作
;; - 空间复杂度：O(1) 不消耗额外栈空间
;; - 内存分配：直接访问现有序对结构，无新对象创建
;; 
;; 数据类型兼容性
;; -------------
;; - 支持所有类型的列表和序对结构
;; - 各元素可以是任意 Scheme 对象
;; - 正确处理嵌套列表和复杂结构
;; cadr 基础测试 - 各种典型场景
(check (cadr '(a b)) => 'b)
(check (cadr '(a b c)) => 'b)
(check (cadr '(1 2 3 4 5)) => 2)
(check-catch 'wrong-type-arg
  (cadr '(a . b))
) ;check-catch
(check (cadr '(a b . rest)) => 'b)
(check (cadr '((a . b) c)) => 'c)
;; cadr 边界测试
(check (cadr '(a b)) => 'b)
(check-catch 'wrong-type-arg
  (cadr '(only))
) ;check-catch
(check (cadr '(pair single)) => 'single)
(check-catch 'wrong-type-arg
  (cadr '(a . b))
) ;check-catch
;; 数据类型边界测试
(check (cadr '(42 string symbol #t))
  =>
  'string
) ;check
(check (cadr '("hello" "world" "test"))
  =>
  "world"
) ;check
(check (cadr '(#t #f #t)) => #f)
(check (cadr '(list vector string))
  =>
  'vector
) ;check
(check (cadr '((a b) (c d) (e f)))
  =>
  '(c d)
) ;check
;; 数值边界测试
(check (cadr '(100 200 300 400 500))
  =>
  200
) ;check
(check (cadr '(1.1 2.2 3.3 4.4 5.5))
  =>
  2.2
) ;check
(check (cadr '(1/2 2/3 3/4)) => 2/3)
(check (cadr '(1.0+2.0i 3.0+4.0i 5.0+6.0i))
  =>
  3.0+4.0i
) ;check
;; 任意对象类型测试
(check (cadr '(#a "string" 42))
  =>
  "string"
) ;check
(check (cadr '(if-cond then-block else-block))
  =>
  'then-block
) ;check
(check (cadr '((lambda (x) x) (lambda (y) y) (lambda (z) z))
       ) ;cadr
  =>
  '(lambda (y) y)
) ;check
;; 构造器创建的结构测试
(check (cadr (list 'a 'b 'c 'd)) => 'b)
(check (cadr (cons 'a (cons 'b (cons 'c '()))))
  =>
  'b
) ;check
(check (cadr (append '() '(a b c)))
  =>
  'b
) ;check
(check (cadr (reverse '(c b a))) => 'b)
;; Unicode和字符串边界测试
(check (cadr '("中文" "测试" "验证"))
  =>
  "测试"
) ;check
(check (cadr '(#\中 #\文 #\字))
  =>
  #\文
) ;check
(check-catch 'wrong-type-arg (cadr '()))
(check-catch 'wrong-type-arg (cadr 123))
(check-catch 'wrong-type-arg
  (cadr "string")
) ;check-catch
(check-catch 'wrong-type-arg (cadr #t))
(check-catch 'wrong-type-arg (cadr #\a))
(check-catch 'wrong-type-arg
  (cadr #(a b))
) ;check-catch
;; 单元素边界异常测试
(check-catch 'wrong-type-arg
  (cadr '(single))
) ;check-catch
(check-catch 'wrong-type-arg
  (cadr '(all))
) ;check-catch
;; 构造器与操作函数链式测试
(check (cadr (list 'car 'cdr 'cons 'append))
  =>
  'cdr
) ;check
;; =======================================
;; [201_12] cadr 补充边界测试和文档完善
;; 根据 201_12.md 要求补充边界值和数据兼容性测试
;; =======================================
;; 边界测试集1：空结构边界
(check-catch 'wrong-type-arg
  (cadr (cons 'a 'b))
) ;check-catch
(check-catch 'wrong-type-arg
  (cadr (cons 'a '()))
) ;check-catch
;; 边界测试集2：极限长度边界
(check (cadr (make-list 1000 'x)) => 'x)
(check (cadr (append '(a) (make-list 999 'x)))
  =>
  'x
) ;check
;; 边界测试集3：特殊对象类型边界
(check (cadr '(#t + #f)) => '+)
(check (cadr '(#t #(1 2 3) #f))
  =>
  #(1 2 3)
) ;check
(check (cadr '(#t #u(255 128) #f))
  =>
  #u(255 128)
) ;check
;; 边界测试集4：Unicode边界测试
(check (cadr '("特殊&符号" "正常字符串")
       ) ;cadr
  =>
  "正常字符串"
) ;check
;; 边界测试集5：复合结构异常边界
(check-catch 'wrong-type-arg
  (cadr (vector 'a 'b))
) ;check-catch
(check-catch 'wrong-number-of-args
  (cadr)
) ;check-catch
(check-catch 'wrong-number-of-args
  (cadr '(a b) '(c d))
) ;check-catch
(check-report)