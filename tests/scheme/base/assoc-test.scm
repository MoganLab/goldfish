(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; assoc 基本功能测试
(check (assoc 'a '((a . 1) (b . 2))) => '(a . 1))
(check (assoc 'b '((a . 1) (b . 2))) => '(b . 2))
(check (assoc 'c '((a . 1) (b . 2))) => #f)
;; 边界1：空列表边界
(check (assoc 'key '()) => #f)
;; 边界2：单元素列表边界
(check (assoc 'k '((k . v))) => '(k . v))
(check (assoc 'missing '((k . v))) => #f)
;; 边界3：字符串键精确匹配边界
(check (assoc "apple" '(("apple" . 1) ("banana" . 2)))
  =>
  '("apple" . 1)
) ;check
(check (assoc "APPLE" '(("apple" . 1) ("banana" . 2)))
  =>
  #f
) ;check
(check (assoc "" '(("" . empty) ("a" . not-empty)))
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
(check (assoc 'key '((key deep nested structure) (other . simple)))
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
(check (assoc 42 '((42 . "int") (42.0 . "float")))
  =>
  '(42 . "int")
) ;check
(check (assoc 42.0 '((42 . "int") (42.0 . "float")))
  =>
  '(42.0 . "float")
) ;check
(check (assoc 3.14 '((1 . a) (3.14 . b) (5 . c)))
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
(check (assoc 'singleton '((singleton . value)))
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
(check (assoc "🚀" '(("🚀" . "rocket") ("🌐" . "globe")))
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
(check (assoc '() '((() . empty-list) ((a) . single)))
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
(check (assoc 'data '((data . #("a" "b" "c")) (list 1 2 3)))
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