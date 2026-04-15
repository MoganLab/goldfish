(import (liii check)
        (liii goldfmt-format)
        (liii raw-string))

(check-set-mode! 'report-failed)

;; format-string
;; 格式化 source string 中的所有 top-level datum。
;;
;; 语法
;; ----
;; (format-string source)
;;
;; 参数
;; ----
;; source : string?
;; 待格式化的 Scheme 源码字符串。
;;
;; 返回值
;; ------
;; string?
;; 返回格式化后的源码文本（始终以换行符结尾）。
;;
;; 说明
;; ----
;; 1. `format-string` 会从 source 中连续读取 datum，直到 EOF
;; 2. 每个 top-level datum 分别调用 `format-datum`
;; 3. 多个 top-level datum 的格式化结果用一个换行连接
;; 4. 输出始终以换行符结尾（符合 POSIX 标准）
;;
;; 示例
;; ----
;; (format-string "(define x 1) (define y 2)")

(check (format-string "(define x 1)")
       => "(define x 1)\n")

(check (format-string "")
       => "")

(check (format-string "   \n  ")
       => "")

;; 注意：多个define之间会自动插入空行（非首个define前）
(check (format-string "(define x 1) (define y 2)")
       => "(define x 1)\n\n(define y 2)\n")

;; 输入中的空行会被保留
(check (format-string "  x\n\n(define y 2)  ")
       => "x\n\n(define y 2)\n")

;; 测试嵌套 quote 形式 '(quote define)
;; '(quote define) 应该保持原样输出，不压缩为 ''define
(check (format-string "'(quote define)")
       => "'(quote define)\n")

;; 测试 '(quote x)
(check (format-string "'(quote x)")
       => "'(quote x)\n")

;; 测试普通 quote 列表
(check (format-string "'(a b c)")
       => "'(a b c)\n")

;; 测试 quote 点对
(check (format-string "'(a b . c)")
       => "'(a b . c)\n")

;; bare syntax object 不应导致 formatter 崩溃
(check (format-string "#_quote")
       => "#_quote\n")

;; quasiquote 内部形式应该还原为 ` , ,@ 语法
(check (format-string "`(a ,b)")
       => "`(a ,b)\n")

(check (format-string "`(a ,@b)")
       => "`(a ,@b)\n")

;; 测试长列表不会被截断为 ...
;; 这是修复 (*s7* 'print-length) 截断问题的回归测试
(check (format-string "(check (assoc 'key50 '((key1 . val1) (key2 . val2) (key3 . val3) (key4 . val4) (key5 . val5) (key6 . val6) (key7 . val7) (key8 . val8) (key9 . val9) (key10 . val10) (key11 . val11) (key12 . val12) (key13 . val13) (key14 . val14) (key15 . val15) (key16 . val16) (key17 . val17) (key18 . val18) (key19 . val19) (key20 . val20) (key21 . val21) (key22 . val22) (key23 . val23) (key24 . val24) (key25 . val25) (key26 . val26) (key27 . val27) (key28 . val28) (key29 . val29) (key30 . val30) (key31 . val31) (key32 . val32) (key33 . val33) (key34 . val34) (key35 . val35) (key36 . val36) (key37 . val37) (key38 . val38) (key39 . val39) (key40 . val40) (key41 . val41) (key42 . val42) (key43 . val43) (key44 . val44) (key45 . val45) (key46 . val46) (key47 . val47) (key48 . val48) (key49 . val49) (key50 . val50))) => '(key50 . val50))")
       => "(check (assoc 'key50\n         '((key1 . val1) (key2 . val2) (key3 . val3) (key4 . val4) (key5 . val5) (key6 . val6) (key7 . val7) (key8 . val8) (key9 . val9) (key10 . val10) (key11 . val11) (key12 . val12) (key13 . val13) (key14 . val14) (key15 . val15) (key16 . val16) (key17 . val17) (key18 . val18) (key19 . val19) (key20 . val20) (key21 . val21) (key22 . val22) (key23 . val23) (key24 . val24) (key25 . val25) (key26 . val26) (key27 . val27) (key28 . val28) (key29 . val29) (key30 . val30) (key31 . val31) (key32 . val32) (key33 . val33) (key34 . val34) (key35 . val35) (key36 . val36) (key37 . val37) (key38 . val38) (key39 . val39) (key40 . val40) (key41 . val41) (key42 . val42) (key43 . val43) (key44 . val44) (key45 . val45) (key46 . val46) (key47 . val47) (key48 . val48) (key49 . val49) (key50 . val50))\n       ) ;assoc\n  =>\n  '(key50 . val50)\n) ;check\n")

;; raw string 应保留原始字面量，不应退化成普通转义字符串
(check (format-string "#\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\"")
       => "#\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\"\n")

;; 多行 raw string 可以和左侧函数保留在同一行
(check (format-string "(display #\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\")")
       => "(display #\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\"\n) ;display\n")

(check-report)
