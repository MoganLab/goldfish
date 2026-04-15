(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; assoc
;; 在关联列表中查找键，使用 equal? 进行比较。这是R7RS标准中最通用的关联列表操作函数。
;; 
;; 语法
;; ----
;; (assoc key alist)
;; 
;; 参数
;; ----
;; key : any
;;     要查找的键值，可以是符号、字符串、数字、列表、点对或其他任意类型对象。
;; 
;; alist : list
;;     关联列表，每个元素都是一个配对（pair）或列表，其中 car 是键，cdr 是值。
;; 
;; 返回值
;; ----
;; list | #f
;;     如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。
;; 
;; 描述
;; ----
;; assoc 是R7RS标准中最通用的关联列表操作函数，使用 equal? 进行键比较，支持所有类型的对象作为键。
;; 与 assq (使用 eq? 比较) 和 assv (使用 eqv? 比较) 相比，assoc 能够完成最复杂的对象匹配。
;; 
;; 边界条件
;; --------
;; - 空列表参数：始终返回 #f
;; - 单元素列表边界：存在匹配时返回单元素配对
;; - 重复键处理：返回第一个匹配值的配对（索引最小值）
;; - 字符串匹配：区分大小写，完全匹配字符串内容
;; - 列表匹配：全结构比较，子列表嵌套也精确匹配
;; - 特殊对象匹配：过程对象、布尔值、字符等统一支持
;; - 复合对象边界：支持复杂嵌套结构作为键和值
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(n*m)，其中 n 为列表长度，m 为键比较复杂度
;; - 空间复杂度：O(1)，无额外内存消耗
;; - 内存共享：成功匹配时共享原始列表内存结构
;; - 通用性：支持所有类型的键值比较
;; - 深层比较：支持复杂嵌套结构的完整匹配
;; 
;; 数据类型兼容性
;; -------------
;; - **字符串类型**：支持大小写敏感的全字符匹配
;; - **列表类型**：支持嵌套列表的完整结构匹配
;; - **数值类型**：支持所有数值类型的等价性比较
;; - **字符类型**：支持ASCII和Unicode字符的精确匹配
;; - **布尔类型**：完全支持 #t 和 #f 的精确匹配
;; - **符号类型**：支持普通符号和关键字符号
;; - **复合对象**：支持任何复杂对象作为键和值
;; 
;; 与其他关联函数的比较
;; --------------------
;; - **assoc vs assq**：assoc 使用 equal?（通用最強），assq 使用 eq?（符号最优）
;; - **assoc vs assv**：assoc 使用 equal?（复合对象最优），assv 使用 eqv?（基础类型最优）
;; - **应用矩阵**：
;;   - 符号键 → 优先 assq
;;   - 数值/字符键 → 优先 assv
;;   - 字符串/列表/复合键 → 优先 assoc
;; 
;; 应用注意
;; --------
;; - 是处理字符串键、列表键等复杂类型键值的唯一选择
;; - 能够正确处理嵌套数据结构作为键值
;; - 适用于配置管理、JSON-like数据结构、序列化处理等场景
;; - 返回结构支持继续链式操作和状态处理
;; - 支持最深/最复杂的数据结构匹配
;; 
;; 限制说明
;; --------
;; - 性能可能不如 assq 和 assv 对于简单类型键
;; - 不支持模糊匹配和通配符匹配
;; - 与 assq 和 assv 共同组成 R7RS 的标准关联查询架构
;; - 需要注意的是会实例化深层结构比较
;; assoc
;; 在关联列表中查找键，使用 equal? 进行比较。这是R7RS标准中最通用的关联列表操作函数。
;; 
;; 语法
;; ----
;; (assoc key alist)
;; 
;; 参数
;; ----
;; key : any
;;     要查找的键值，可以是符号、字符串、数字、列表、点对或其他任意类型对象。
;; 
;; alist : list
;;     关联列表，每个元素都是一个配对（pair）或列表，其中 car 是键，cdr 是值。
;; 
;; 返回值
;; ----
;; list | #f
;;     如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。
;; 
;; 描述
;; ----
;; assoc 是R7RS标准中最通用的关联列表操作函数，使用 equal? 进行键比较，支持所有类型的对象作为键。
;; 与 assq (使用 eq? 比较) 和 assv (使用 eqv? 比较) 相比，assoc 能够完成最复杂的对象匹配。
;; 
;; 边界条件
;; --------
;; - 空列表参数：始终返回 #f
;; - 单元素列表边界：存在匹配时返回单元素配对
;; - 重复键处理：返回第一个匹配值的配对（索引最小值）
;; - 字符串匹配：区分大小写，完全匹配字符串内容
;; - 列表匹配：全结构比较，子列表嵌套也精确匹配
;; - 特殊对象匹配：过程对象、布尔值、字符等统一支持
;; - 复合对象边界：支持复杂嵌套结构作为键和值
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(n*m)，其中 n 为列表长度，m 为键比较复杂度
;; - 空间复杂度：O(1)，无额外内存消耗
;; - 内存共享：成功匹配时共享原始列表内存结构
;; - 通用性：支持所有类型的键值比较
;; - 深层比较：支持复杂嵌套结构的完整匹配
;; 
;; 数据类型兼容性
;; -------------
;; - **字符串类型**：支持大小写敏感的全字符匹配
;; - **列表类型**：支持嵌套列表的完整结构匹配
;; - **数值类型**：支持所有数值类型的等价性比较
;; - **字符类型**：支持ASCII和Unicode字符的精确匹配
;; - **布尔类型**：完全支持 #t 和 #f 的精确匹配
;; - **符号类型**：支持普通符号和关键字符号
;; - **复合对象**：支持任何复杂对象作为键和值
;; 
;; 与其他关联函数的比较
;; --------------------
;; - **assoc vs assq**：assoc 使用 equal?（通用最強），assq 使用 eq?（符号最优）
;; - **assoc vs assv**：assoc 使用 equal?（复合对象最优），assv 使用 eqv?（基础类型最优）
;; - **应用矩阵**：
;;   - 符号键 → 优先 assq
;;   - 数值/字符键 → 优先 assv
;;   - 字符串/列表/复合键 → 优先 assoc
;; 
;; 应用注意
;; --------
;; - 是处理字符串键、列表键等复杂类型键值的唯一选择
;; - 能够正确处理嵌套数据结构作为键值
;; - 适用于配置管理、JSON-like数据结构、序列化处理等场景
;; - 返回结构支持继续链式操作和状态处理
;; - 支持最深/最复杂的数据结构匹配
;; 
;; 限制说明
;; --------
;; - 性能可能不如 assq 和 assv 对于简单类型键
;; - 不支持模糊匹配和通配符匹配
;; - 与 assq 和 assv 共同组成 R7RS 的标准关联查询架构
;; - 需要注意的是会实例化深层结构比较
;; assoc
;; 在关联列表中查找键，使用 equal? 进行比较。这是R7RS标准中最通用的关联列表操作函数。
;; 
;; 语法
;; ----
;; (assoc key alist)
;; 
;; 参数
;; ----
;; key : any
;;     要查找的键值，可以是符号、字符串、数字、列表、点对或其他任意类型对象。
;; 
;; alist : list
;;     关联列表，每个元素都是一个配对（pair）或列表，其中 car 是键，cdr 是值。
;; 
;; 返回值
;; ----
;; list | #f
;;     如果在关联列表中找到匹配的键，返回对应的配对；如果未找到匹配项，返回 #f。
;; 
;; 描述
;; ----
;; assoc 是R7RS标准中最通用的关联列表操作函数，使用 equal? 进行键比较，支持所有类型的对象作为键。
;; 与 assq (使用 eq? 比较) 和 assv (使用 eqv? 比较) 相比，assoc 能够完成最复杂的对象匹配。
;; 
;; 边界条件
;; --------
;; - 空列表参数：始终返回 #f
;; - 单元素列表边界：存在匹配时返回单元素配对
;; - 重复键处理：返回第一个匹配值的配对（索引最小值）
;; - 字符串匹配：区分大小写，完全匹配字符串内容
;; - 列表匹配：全结构比较，子列表嵌套也精确匹配
;; - 特殊对象匹配：过程对象、布尔值、字符等统一支持
;; - 复合对象边界：支持复杂嵌套结构作为键和值
;; 
;; 性能特征
;; --------
;; - 时间复杂度：O(n*m)，其中 n 为列表长度，m 为键比较复杂度
;; - 空间复杂度：O(1)，无额外内存消耗
;; - 内存共享：成功匹配时共享原始列表内存结构
;; - 通用性：支持所有类型的键值比较
;; - 深层比较：支持复杂嵌套结构的完整匹配
;; 
;; 数据类型兼容性
;; -------------
;; - **字符串类型**：支持大小写敏感的全字符匹配
;; - **列表类型**：支持嵌套列表的完整结构匹配
;; - **数值类型**：支持所有数值类型的等价性比较
;; - **字符类型**：支持ASCII和Unicode字符的精确匹配
;; - **布尔类型**：完全支持 #t 和 #f 的精确匹配
;; - **符号类型**：支持普通符号和关键字符号
;; - **复合对象**：支持任何复杂对象作为键和值
;; 
;; 与其他关联函数的比较
;; --------------------
;; - **assoc vs assq**：assoc 使用 equal?（通用最強），assq 使用 eq?（符号最优）
;; - **assoc vs assv**：assoc 使用 equal?（复合对象最优），assv 使用 eqv?（基础类型最优）
;; - **应用矩阵**：
;;   - 符号键 → 优先 assq
;;   - 数值/字符键 → 优先 assv
;;   - 字符串/列表/复合键 → 优先 assoc
;; 
;; 应用注意
;; --------
;; - 是处理字符串键、列表键等复杂类型键值的唯一选择
;; - 能够正确处理嵌套数据结构作为键值
;; - 适用于配置管理、JSON-like数据结构、序列化处理等场景
;; - 返回结构支持继续链式操作和状态处理
;; - 支持最深/最复杂的数据结构匹配
;; 
;; 限制说明
;; --------
;; - 性能可能不如 assq 和 assv 对于简单类型键
;; - 不支持模糊匹配和通配符匹配
;; - 与 assq 和 assv 共同组成 R7RS 的标准关联查询架构
;; - 需要注意的是会实例化深层结构比较
;; assoc 基本功能测试
(check (assoc 'a '((a . 1) (b . 2)))
  =>
  '(a . 1)
) ;check
(check (assoc 'b '((a . 1) (b . 2)))
  =>
  '(b . 2)
) ;check
(check (assoc 'c '((a . 1) (b . 2)))
  =>
  #f
) ;check
;; 边界1：空列表边界
(check (assoc 'key '()) => #f)
;; 边界2：单元素列表边界
(check (assoc 'k '((k . v)))
  =>
  '(k . v)
) ;check
(check (assoc 'missing '((k . v)))
  =>
  #f
) ;check
;; 边界3：字符串键精确匹配边界
(check (assoc "apple"
         '(("apple" . 1) ("banana" . 2))
       ) ;assoc
  =>
  '("apple" . 1)
) ;check
(check (assoc "APPLE"
         '(("apple" . 1) ("banana" . 2))
       ) ;assoc
  =>
  #f
) ;check
(check (assoc ""
         '(("" . empty) ("a" . not-empty))
       ) ;assoc
  =>
  '("" . empty)
) ;check
;; 边界4：列表键精确匹配边界
(check (assoc '(1 2)
         '(((1 2) . "list-key-1") ((3 4) . "list-key-2"))
       ) ;assoc
  =>
  '((1 2) . "list-key-1")
) ;check
(check (assoc '(list 1 2)
         '(((list 1 2) . quoted) ((1 2) . unquoted))
       ) ;assoc
  =>
  '((list 1 2) . quoted)
) ;check
;; 边界5：嵌套结构作为键边界
(check (assoc 'key
         '((key deep nested structure) (other . simple))
       ) ;assoc
  =>
  '(key deep nested structure)
) ;check
(check (assoc 'nested
         '((("key" . val) ((key) . "the key")) (missing . other))
       ) ;assoc
  =>
  #f
) ;check
;; 边界6：重复键处理边界
(check (assoc "test"
         '(("test" . first) ("test" . second) ("test" . third))
       ) ;assoc
  =>
  '("test" . first)
) ;check
(check (assoc '(a b)
         '(((a b) . one) ((a b) . two) ((c d) . three))
       ) ;assoc
  =>
  '((a b) . one)
) ;check
;; 边界7：数值等价性边界测试
(check (assoc 42
         '((42 . "int") (42.0 . "float"))
       ) ;assoc
  =>
  '(42 . "int")
) ;check
(check (assoc 42.0
         '((42 . "int") (42.0 . "float"))
       ) ;assoc
  =>
  '(42.0 . "float")
) ;check
(check (assoc 3.14
         '((1 . a) (3.14 . b) (5 . c))
       ) ;assoc
  =>
  '(3.14 . b)
) ;check
;; 边界8：复杂复合对象作为键边界
(check (assoc '((a b) c)
         '(("((a b) c)" . string) (((a b) c) . list) ((()) . empty))
       ) ;assoc
  =>
  '(((a b) c) . list)
) ;check
(check (assoc #(1 2 3)
         '((#(1 2 3) . vector-value) (#(3 4 5) . another))
       ) ;assoc
  =>
  '(#(1 2 3) . vector-value)
) ;check
;; 字符串详细边界测试
(check (assoc "" '(("" . empty-string)))
  =>
  '("" . empty-string)
) ;check
(check (assoc "特殊字符"
         '(("特殊字符" . "unicode string"))
       ) ;assoc
  =>
  '("特殊字符" . "unicode string")
) ;check
;; 补充assoc完整边界测试
;; 边界：空列表和单元素边界
(check (assoc 'missing '()) => #f)
(check (assoc 'singleton
         '((singleton . value))
       ) ;assoc
  =>
  '(singleton . value)
) ;check
;; 边界：Unicode字符串精确匹配
(check (assoc "中文"
         '(("中文" . "中文值") ("english" . "英文值"))
       ) ;assoc
  =>
  '("中文" . "中文值")
) ;check
(check (assoc "🚀"
         '(("🚀" . "rocket") ("🌐" . "globe"))
       ) ;assoc
  =>
  '("🚀" . "rocket")
) ;check
;; 边界：空字符串和特殊字符
(check (assoc "" '(("" . empty-string)))
  =>
  '("" . empty-string)
) ;check
(check (assoc "TeSt"
         '(("test" . lowercase) ("TeSt" . mixed) ("TEST" . uppercase))
       ) ;assoc
  =>
  '("TeSt" . mixed)
) ;check
;; 边界：列表结构作为键
(check (assoc '()
         '((() . empty-list) ((a) . single))
       ) ;assoc
  =>
  '(() . empty-list)
) ;check
(check (assoc '(nested list)
         '(((nested list) . "complex key") ((simple) . "basic key"))
       ) ;assoc
  =>
  '((nested list) . "complex key")
) ;check
;; 扩展assoc边界测试，满足至少8个边界测试用例要求
;; 边界9：极大关联列表性能测试
(check (assoc 'key50
         '((key1 . val1) (key2 . val2) (key3 . val3) (key4 . val4) (key5 . val5) (key6 . val6) (key7 . val7) (key8 . val8) (key9 . val9) (key10 . val10) (key11 . val11) (key12 . val12) (key13 . val13) (key14 . val14) (key15 . val15) (key16 . val16) (key17 . val17) (key18 . val18) (key19 . val19) (key20 . val20) (key21 . val21) (key22 . val22) (key23 . val23) (key24 . val24) (key25 . val25) (key26 . val26) (key27 . val27) (key28 . val28) (key29 . val29) (key30 . val30) (key31 . val31) (key32 . val32) (key33 . val33) (key34 . val34) (key35 . val35) (key36 . val36) (key37 . val37) (key38 . val38) (key39 . val39) (key40 . val40) (key41 . val41) (key42 . val42) (key43 . val43) (key44 . val44) (key45 . val45) (key46 . val46) (key47 . val47) (key48 . val48) (key49 . val49) (key50 . val50))
       ) ;assoc
  =>
  '(key50 . val50)
) ;check
;; 边界10：深层嵌套结构键值匹配测试
(check (assoc '((a b) (c d))
         '((((a b) (c d)) . "nested keys") (((x y) (z w)) . "other"))
       ) ;assoc
  =>
  '(((a b) (c d)) . "nested keys")
) ;check
;; 边界11：混合数据类型的键测试
(check (assoc 3.14159
         '((1 . "one") (3.14159 . "pi") (2.71828 . "e"))
       ) ;assoc
  =>
  '(3.14159 . "pi")
) ;check
(check (assoc #f
         '((#t . "true") (#f . "false") (maybe . "unknown"))
       ) ;assoc
  =>
  '(#f . "false")
) ;check
;; 边界12：特殊字符和Unicode字符串键测试
(check (assoc "测试"
         '(("中文" . "中文值") ("测试" . "测试值") ("英文" . "english"))
       ) ;assoc
  =>
  '("测试" . "测试值")
) ;check
(check (assoc "特殊字符@#$%"
         '(("normal" . "普通") ("特殊字符@#$%" . "special") ("unicode🎉" . "emoji"))
       ) ;assoc
  =>
  '("特殊字符@#$%" . "special")
) ;check
;; 边界13：过程对象和布尔值作为键测试
(let ((test-proc (lambda (x) x)))
  (check (assoc test-proc
           (list (cons test-proc "procedure")
             (cons 'not-this "symbol")
           ) ;list
         ) ;assoc
    =>
    (cons test-proc "procedure")
  ) ;check
) ;let
;; 边界14：空列表和原子值边界测试
(check (assoc 'missing '()) => #f)
(check (assoc 'key '((key))) => '(key))
(check (assoc 'symbol
         '((symbol . #t) (number . 42) (string . "test"))
       ) ;assoc
  =>
  '(symbol . #t)
) ;check
;; 边界15：存储过程和复杂对象的值测试
(check (assoc 'data
         '((data . #("a" "b" "c")) (list 1 2 3))
       ) ;assoc
  =>
  '(data . #("a" "b" "c"))
) ;check
;; 边界16：重复前缀匹配测试
(check (assoc "prefix"
         '(("prefix-match" . 1) ("prefix" . 2) ("prefix-long" . 3))
       ) ;assoc
  =>
  '("prefix" . 2)
) ;check
(check (assoc 'partial
         '((partial . 1) (partial-match . 2) (partially . 3))
       ) ;assoc
  =>
  '(partial . 1)
) ;check
(check-report)