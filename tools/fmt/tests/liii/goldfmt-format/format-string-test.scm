(import (liii check)
  (liii goldfmt-format)
  (liii goldfmt-scan)
  (liii os)
  (liii raw-string)
  (srfi srfi-13)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file filename)
  (let ((local-path (string-append "tests/resources/" filename))
        (abs-path (string-append "tools/fmt/tests/resources/" filename)))
    (if (access local-path 'R_OK)
        local-path
        abs-path)
  ) ;let
) ;define

(define (format-first-node-file filename)
  (let ((nodes (scan-file (resource-file filename))))
    (call-with-values
      (lambda () (format-node (vector-ref nodes 0) 0))
      (lambda (text positioned-node) text)
    ) ;call-with-values
  ) ;let
) ;define

(define (string-includes? text pattern)
  (if (string-contains text pattern) #t #f)
) ;define

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
  =>
  "(define x 1)\n"
) ;check

(check (format-string "") => "")

(check (format-string "   \n  ") => "")

;; 注意：多个define之间会自动插入空行（非首个define前）
(check (format-string "(define x 1) (define y 2)"
       ) ;format-string
  =>
  "(define x 1)\n\n(define y 2)\n"
) ;check

;; 输入中的空行会被保留
(check (format-string "  x\n\n(define y 2)  ")
  =>
  "x\n\n(define y 2)\n"
) ;check

;; 测试嵌套 quote 形式 '(quote define)
;; '(quote define) 应该保持原样输出，不压缩为 ''define
(check (format-string "'(quote define)")
  =>
  "'(quote define)\n"
) ;check

;; 测试 '(quote x)
(check (format-string "'(quote x)")
  =>
  "'(quote x)\n"
) ;check

;; 测试普通 quote 列表
(check (format-string "'(a b c)")
  =>
  "'(a b c)\n"
) ;check

;; 测试 quote 点对
(check (format-string "'(a b . c)")
  =>
  "'(a b . c)\n"
) ;check

;; bare syntax object 不应导致 formatter 崩溃
(check (format-string "#_quote")
  =>
  "#_quote\n"
) ;check

;; quasiquote 内部形式应该还原为 ` , ,@ 语法
(check (format-string "`(a ,b)")
  =>
  "`(a ,b)\n"
) ;check

(check (format-string "`(a ,@b)")
  =>
  "`(a ,@b)\n"
) ;check

;; quote/quasiquote 内部由 scan-file 注入的 (*newline* n) 应展开为真实空行
(check (format-first-node-file "007_01.scm")
  =>
  "`(begin\n\n   (define ,name ,value))"
) ;check

(check (format-first-node-file "007_02.scm")
  =>
  "'(begin\n\n   (define x 1))"
) ;check

(check (format-first-node-file "007_03.scm")
  =>
  "`(begin\n\n\n   (define ,name ,value))"
) ;check

(check (format-first-node-file "007_04.scm")
  =>
  "`(begin\n   (define x ,a)\n\n   (define y ,b)\n   (define z ,c))"
) ;check

(check (format-datum '(quote #(a (*newline* 1) b)))
  =>
  "'#(a\n\n   b)"
) ;check

;; 超过 max-inline-length 的 reader datum 应进入 head-aware 多行排版
(check (format-string "`(begin (define ,name ,value) ,@(map f xs) (quote done) (another-long-form alpha beta gamma delta epsilon))"
       ) ;format-string
  =>
  "`(begin\n   (define ,name ,value)\n   ,@(map f xs)\n   (quote done)\n   (another-long-form alpha beta gamma delta epsilon))\n"
) ;check

(let ((typed-lambda-text (format-first-node-file "007_05.scm")))
  (check (string-includes? typed-lambda-text "(lambda ,new-args") => #t)
  (check (string-includes? typed-lambda-text ",@(map (lambda (arg)") => #t)
  (check (string-includes? typed-lambda-text "(if (pair? arg)") => #t)
  (check (string-includes? typed-lambda-text "`(unless (,(cadr arg) ,(car arg))") => #t)
  (check (string-includes? typed-lambda-text "(lambda\n") => #f)
  (check (string-includes? typed-lambda-text "(map\n") => #f)
  (check (string-includes? typed-lambda-text "(if\n") => #f)
  (check (string-includes? typed-lambda-text "(unless\n") => #f)
) ;let

;; keyword datum 不能被误当成 keyword argument
(check (format-datum '(:trie)) => "(:trie)")

;; 测试长列表不会被截断为 ...
;; 这是修复 (*s7* 'print-length) 截断问题的回归测试
(check (format-string "(check (assoc 'key50 '((key1 . val1) (key2 . val2) (key3 . val3) (key4 . val4) (key5 . val5) (key6 . val6) (key7 . val7) (key8 . val8) (key9 . val9) (key10 . val10) (key11 . val11) (key12 . val12) (key13 . val13) (key14 . val14) (key15 . val15) (key16 . val16) (key17 . val17) (key18 . val18) (key19 . val19) (key20 . val20) (key21 . val21) (key22 . val22) (key23 . val23) (key24 . val24) (key25 . val25) (key26 . val26) (key27 . val27) (key28 . val28) (key29 . val29) (key30 . val30) (key31 . val31) (key32 . val32) (key33 . val33) (key34 . val34) (key35 . val35) (key36 . val36) (key37 . val37) (key38 . val38) (key39 . val39) (key40 . val40) (key41 . val41) (key42 . val42) (key43 . val43) (key44 . val44) (key45 . val45) (key46 . val46) (key47 . val47) (key48 . val48) (key49 . val49) (key50 . val50))) => '(key50 . val50))"
       ) ;format-string
  =>
  "(check (assoc 'key50\n         '((key1 . val1)\n           (key2 . val2)\n           (key3 . val3)\n           (key4 . val4)\n           (key5 . val5)\n           (key6 . val6)\n           (key7 . val7)\n           (key8 . val8)\n           (key9 . val9)\n           (key10 . val10)\n           (key11 . val11)\n           (key12 . val12)\n           (key13 . val13)\n           (key14 . val14)\n           (key15 . val15)\n           (key16 . val16)\n           (key17 . val17)\n           (key18 . val18)\n           (key19 . val19)\n           (key20 . val20)\n           (key21 . val21)\n           (key22 . val22)\n           (key23 . val23)\n           (key24 . val24)\n           (key25 . val25)\n           (key26 . val26)\n           (key27 . val27)\n           (key28 . val28)\n           (key29 . val29)\n           (key30 . val30)\n           (key31 . val31)\n           (key32 . val32)\n           (key33 . val33)\n           (key34 . val34)\n           (key35 . val35)\n           (key36 . val36)\n           (key37 . val37)\n           (key38 . val38)\n           (key39 . val39)\n           (key40 . val40)\n           (key41 . val41)\n           (key42 . val42)\n           (key43 . val43)\n           (key44 . val44)\n           (key45 . val45)\n           (key46 . val46)\n           (key47 . val47)\n           (key48 . val48)\n           (key49 . val49)\n           (key50 . val50))\n       ) ;assoc\n  =>\n  '(key50 . val50)\n) ;check\n"
) ;check

;; raw string 应保留原始字面量，不应退化成普通转义字符串
(check (format-string "#\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\""
       ) ;format-string
  =>
  "#\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\"\n"
) ;check

;; char literal 应保留原始写法，尤其是 #\x 前缀不能在 fmt 后变成别的形式
(check (format-string "(list #\\x2000 #\\中 #\\space)")
  =>
  "(list #\\x2000 #\\中 #\\space)\n"
) ;check

;; quoted char literal 不应泄漏内部 (*char-literal* ...) 表示
(check (format-string "'((#\\a . lowercase) (#\\space . blank))")
  =>
  "'((#\\a . lowercase) (#\\space . blank))\n"
) ;check

;; 多行 raw string 可以和左侧函数保留在同一行
(check (format-string "(display #\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\")"
       ) ;format-string
  =>
  "(display #\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\"\n) ;display\n"
) ;check

;; quoted reader datum 中的 multiline vector 不能丢失后续元素
(check (format-string "(check (f '((\"messages\" . #(((\"role\" . \"user\") (\"content\" . #(((\"text\" . \"1\") (\"type\" . \"text\")) ((\"text\" . \"2\") (\"type\" . \"text\"))))) ((\"role\" . \"user\") (\"content\" . \"中文\")))))) => \"ok\")"
       ) ;format-string
  =>
  "(check (f '((\"messages\"\n             . #(((\"role\" . \"user\")\n                  (\"content\"\n                   . #(((\"text\" . \"1\") (\"type\" . \"text\"))\n                       ((\"text\" . \"2\") (\"type\" . \"text\")))))\n                       ((\"role\" . \"user\") (\"content\" . \"中文\")))))\n       ) ;f\n  =>\n  \"ok\"\n) ;check\n"
) ;check

(check-report)
