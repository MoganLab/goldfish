(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list-set!
;; 修改列表指定索引位置的元素值。该函数会直接修改原始列表对象，是R7RS标准的基本列表操作函数之一。
;; 
;; 语法
;; ----
;; (list-set! list k obj)
;; 
;; 参数
;; ----
;; list : pair?
;;     要被修改的列表对象。可以是普通列表、嵌套列表或点对结构。
;;     必须是可修改的pair类型对象，不能是空列表或其他原子类型。
;; 
;; k : exact?
;;     非负的精确整数，表示要修改的元素索引位置。索引从0开始。
;;     必须满足 0 <= k < (length list)。
;; 
;; obj : any
;;     要设置的新值，可以是任何类型的Scheme对象，包括数字、字符串、符号、布尔值、过程等。
;; 
;; 返回值
;; ------
;; unspecified
;;     根据R7RS规范，返回未指定的值。
;; 
;; 说明
;; ----
;; 1. 这是一个变异操作，会直接修改原始列表对象的内存内容
;; 2. 索引从0开始计数，与list-ref等其他列表函数保持一致
;; 3. 修改后原始对象的引用仍然指向同一个内存位置
;; 4. 适用于所有可修改的列表结构：普通列表、嵌套列表、点对结构
;; 5. 不会影响列表其他元素的位置或结构
;; 
;; 边界条件
;; --------
;; - k = 0: 修改列表第一个元素
;; - k = list.length - 1: 修改列表最后一个元素
;; - k < 0: 抛出out-of-range异常
;; - k >= list.length: 抛出out-of-range异常
;; - 空列表参数：抛出wrong-type-arg异常
;; - 非列表参数：抛出wrong-type-arg异常
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(k)，需要遍历前k个元素
;; - 空间复杂度：O(1)，不消耗额外内存
;; - 内存分配：无额外内存分配，直接修改原始结构
;; - 影响范围：仅修改指定位置的元素值，不影响列表结构
;; 
;; 数据类型兼容性
;; ---------------
;; - 支持任意类型的列表元素替换：
;;   - 基本类型：数字、字符串、字符、符号、布尔值
;;   - 复杂类型：列表、点对结构、向量、过程
;;   - 特殊类型：空列表、嵌套结构、lambda构造器
;; - 支持深度嵌套列表的结构修改
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;;     当list参数不是pair?类型或参数数量错误时抛出。
;; out-of-range
;;     当索引k为负数、超出列表长度或参数类型错误时抛出。
;; 
;; 影响范围
;; --------
;; - 该操作会直接影响使用同一引用的所有代码位置
;; - 修改后原始对象的内容立即发生变化
;; - 列表长度和结构保持不变，仅修改特定位置的值
;; - 谨慎使用，可能破坏特殊列表的不变量
;; 
;; 注意事项
;; --------
;; - 列表参数必须是非空列表或可修改的pair对象
;; - 修改后直接作用于原始列表，不创建新对象
;; - 可以通过校验list-ref来验证修改的正确性
;; - Rust-like安全模型：严格的边界检查和类型验证
;; list-set!
;; 修改列表指定索引位置的元素值。该函数会直接修改原始列表对象，是R7RS标准的基本列表操作函数之一。
;; 
;; 语法
;; ----
;; (list-set! list k obj)
;; 
;; 参数
;; ----
;; list : pair?
;;     要被修改的列表对象。可以是普通列表、嵌套列表或点对结构。
;;     必须是可修改的pair类型对象，不能是空列表或其他原子类型。
;; 
;; k : exact?
;;     非负的精确整数，表示要修改的元素索引位置。索引从0开始。
;;     必须满足 0 <= k < (length list)。
;; 
;; obj : any
;;     要设置的新值，可以是任何类型的Scheme对象，包括数字、字符串、符号、布尔值、过程等。
;; 
;; 返回值
;; ------
;; unspecified
;;     根据R7RS规范，返回未指定的值。
;; 
;; 说明
;; ----
;; 1. 这是一个变异操作，会直接修改原始列表对象的内存内容
;; 2. 索引从0开始计数，与list-ref等其他列表函数保持一致
;; 3. 修改后原始对象的引用仍然指向同一个内存位置
;; 4. 适用于所有可修改的列表结构：普通列表、嵌套列表、点对结构
;; 5. 不会影响列表其他元素的位置或结构
;; 
;; 边界条件
;; --------
;; - k = 0: 修改列表第一个元素
;; - k = list.length - 1: 修改列表最后一个元素
;; - k < 0: 抛出out-of-range异常
;; - k >= list.length: 抛出out-of-range异常
;; - 空列表参数：抛出wrong-type-arg异常
;; - 非列表参数：抛出wrong-type-arg异常
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(k)，需要遍历前k个元素
;; - 空间复杂度：O(1)，不消耗额外内存
;; - 内存分配：无额外内存分配，直接修改原始结构
;; - 影响范围：仅修改指定位置的元素值，不影响列表结构
;; 
;; 数据类型兼容性
;; ---------------
;; - 支持任意类型的列表元素替换：
;;   - 基本类型：数字、字符串、字符、符号、布尔值
;;   - 复杂类型：列表、点对结构、向量、过程
;;   - 特殊类型：空列表、嵌套结构、lambda构造器
;; - 支持深度嵌套列表的结构修改
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;;     当list参数不是pair?类型或参数数量错误时抛出。
;; out-of-range
;;     当索引k为负数、超出列表长度或参数类型错误时抛出。
;; 
;; 影响范围
;; --------
;; - 该操作会直接影响使用同一引用的所有代码位置
;; - 修改后原始对象的内容立即发生变化
;; - 列表长度和结构保持不变，仅修改特定位置的值
;; - 谨慎使用，可能破坏特殊列表的不变量
;; 
;; 注意事项
;; --------
;; - 列表参数必须是非空列表或可修改的pair对象
;; - 修改后直接作用于原始列表，不创建新对象
;; - 可以通过校验list-ref来验证修改的正确性
;; - Rust-like安全模型：严格的边界检查和类型验证
;; list-set!
;; 修改列表指定索引位置的元素值。该函数会直接修改原始列表对象，是R7RS标准的基本列表操作函数之一。
;; 
;; 语法
;; ----
;; (list-set! list k obj)
;; 
;; 参数
;; ----
;; list : pair?
;;     要被修改的列表对象。可以是普通列表、嵌套列表或点对结构。
;;     必须是可修改的pair类型对象，不能是空列表或其他原子类型。
;; 
;; k : exact?
;;     非负的精确整数，表示要修改的元素索引位置。索引从0开始。
;;     必须满足 0 <= k < (length list)。
;; 
;; obj : any
;;     要设置的新值，可以是任何类型的Scheme对象，包括数字、字符串、符号、布尔值、过程等。
;; 
;; 返回值
;; ------
;; unspecified
;;     根据R7RS规范，返回未指定的值。
;; 
;; 说明
;; ----
;; 1. 这是一个变异操作，会直接修改原始列表对象的内存内容
;; 2. 索引从0开始计数，与list-ref等其他列表函数保持一致
;; 3. 修改后原始对象的引用仍然指向同一个内存位置
;; 4. 适用于所有可修改的列表结构：普通列表、嵌套列表、点对结构
;; 5. 不会影响列表其他元素的位置或结构
;; 
;; 边界条件
;; --------
;; - k = 0: 修改列表第一个元素
;; - k = list.length - 1: 修改列表最后一个元素
;; - k < 0: 抛出out-of-range异常
;; - k >= list.length: 抛出out-of-range异常
;; - 空列表参数：抛出wrong-type-arg异常
;; - 非列表参数：抛出wrong-type-arg异常
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(k)，需要遍历前k个元素
;; - 空间复杂度：O(1)，不消耗额外内存
;; - 内存分配：无额外内存分配，直接修改原始结构
;; - 影响范围：仅修改指定位置的元素值，不影响列表结构
;; 
;; 数据类型兼容性
;; ---------------
;; - 支持任意类型的列表元素替换：
;;   - 基本类型：数字、字符串、字符、符号、布尔值
;;   - 复杂类型：列表、点对结构、向量、过程
;;   - 特殊类型：空列表、嵌套结构、lambda构造器
;; - 支持深度嵌套列表的结构修改
;; 
;; 错误处理
;; --------
;; wrong-type-arg
;;     当list参数不是pair?类型或参数数量错误时抛出。
;; out-of-range
;;     当索引k为负数、超出列表长度或参数类型错误时抛出。
;; 
;; 影响范围
;; --------
;; - 该操作会直接影响使用同一引用的所有代码位置
;; - 修改后原始对象的内容立即发生变化
;; - 列表长度和结构保持不变，仅修改特定位置的值
;; - 谨慎使用，可能破坏特殊列表的不变量
;; 
;; 注意事项
;; --------
;; - 列表参数必须是非空列表或可修改的pair对象
;; - 修改后直接作用于原始列表，不创建新对象
;; - 可以通过校验list-ref来验证修改的正确性
;; - Rust-like安全模型：严格的边界检查和类型验证
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
(let ((lst (list #(1 2 3) #u8(255 128))))
  (list-set! lst 0 #(4 5 6))
  (list-set! lst 1 #u8(100 200))
  (check lst => '(#(4 5 6) #u8(100 200)))
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