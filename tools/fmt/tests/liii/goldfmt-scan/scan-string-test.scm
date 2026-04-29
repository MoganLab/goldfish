(import (liii check))
(import (liii goldfmt-scan))
(import (liii goldfmt-record))
(import (liii raw-string))

(check-set-mode! 'report-failed)

;; 测试 scan-string：两个顶层表达式
(let ((results (scan-string #"CODE"(+ 1 2) (+ 3 4)"CODE"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 2)
  ;; 检查第一个顶层表达式
  (let ((first (vector-ref results 0)))
    (check (env? first) => #t)
    (check (env-tag-name first) => "+")
    (check (env-depth first) => 0)
    (check (vector-length (env-children first))
      =>
      2
    ) ;check
  ) ;let
  ;; 检查第二个顶层表达式
  (let ((second (vector-ref results 1)))
    (check (env? second) => #t)
    (check (env-tag-name second) => "+")
    (check (env-depth second) => 0)
    (check (vector-length (env-children second))
      =>
      2
    ) ;check
  ) ;let
) ;let

;; 测试 scan-string：三个顶层表达式（混合 atom 和 env）
(let ((results (scan-string #"CODE"42 (define x 1) "hello""CODE"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 3)
  ;; 第一个是 atom
  (check (atom? (vector-ref results 0))
    =>
    #t
  ) ;check
  (check (atom-value (vector-ref results 0))
    =>
    42
  ) ;check
  (check (atom-depth (vector-ref results 0))
    =>
    0
  ) ;check
  ;; 第二个是 env
  (check (env? (vector-ref results 1))
    =>
    #t
  ) ;check
  (check (env-tag-name (vector-ref results 1))
    =>
    "define"
  ) ;check
  (check (env-depth (vector-ref results 1))
    =>
    0
  ) ;check
  ;; 第三个是 atom
  (check (atom? (vector-ref results 2))
    =>
    #t
  ) ;check
  (check (atom-value (vector-ref results 2))
    =>
    "hello"
  ) ;check
  (check (atom-depth (vector-ref results 2))
    =>
    0
  ) ;check
) ;let

;; 测试 scan-string：空字符串
(let ((results (scan-string #"""")))
  (check (vector? results) => #t)
  (check (vector-length results) => 0)
) ;let

;; 测试 scan-string：只有空白字符
(let ((results (scan-string #"CODE"
   
	  
"CODE"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 0)
) ;let

;; 测试 scan-string：单个表达式
(let ((results (scan-string #"CODE"(define x 1)"CODE")
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let ((first (vector-ref results 0)))
    (check (env? first) => #t)
    (check (env-tag-name first) => "define")
    (check (env-depth first) => 0)
  ) ;let
) ;let

;; 字符字面量需要保留原始 source，避免 fmt 把 #\x 形式改写成 #\ 形式
(let* ((results (scan-string "(list #\\x2000 #\\中 #\\space)"))
       (node (vector-ref results 0))
       (children (env-children node))
       (char-x (atom-value (vector-ref children 0)))
       (char-zh (atom-value (vector-ref children 1)))
       (char-space (atom-value (vector-ref children 2)))
      ) ;
  (check (char-literal? char-x) => #t)
  (check (char-literal-source char-x) => "#\\x2000")
  (check (char-literal-value char-x) => #\x2000)
  (check (char-literal? char-zh) => #t)
  (check (char-literal-source char-zh) => "#\\中")
  (check (char-literal-value char-zh) => #\中)
  (check (char-literal? char-space) => #t)
  (check (char-literal-source char-space) => "#\\space")
  (check (char-literal-value char-space) => #\space)
) ;let*

;; 测试 scan-string：嵌套结构
(let ((results (scan-string #"CODE"(if (a b) c d)"CODE"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let ((result (vector-ref results 0)))
    (check (env? result) => #t)
    (check (env-tag-name result) => "if")
    (check (env-depth result) => 0)
    (check (vector-length (env-children result))
      =>
      3
    ) ;check
  ) ;let
) ;let

;; 测试 scan-string：多行表达式
(let ((results (scan-string #"CODE"
(define x 1)
(define y 2)
"CODE"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 2)
  (let ((first (vector-ref results 0))
        (second (vector-ref results 1))
       ) ;
    (check (env-tag-name first) => "define")
    (check (env-tag-name second)
      =>
      "define"
    ) ;check
    (check (env-depth first) => 0)
    (check (env-depth second) => 0)
  ) ;let
) ;let

;; 测试 scan-string：单行注释处理（*comment* 结构）
(let ((results (scan-string "(*comment* \"这是一个注释\")"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let ((first (vector-ref results 0)))
    (check (env? first) => #t)
    (check (env-tag-name first)
      =>
      "*comment*"
    ) ;check
    (check (env-depth first) => 0)
    (check (vector-length (env-children first))
      =>
      1
    ) ;check
    ;; 检查注释内容是字符串 atom
    (let ((content (vector-ref (env-children first) 0)
          ) ;content
         ) ;
      (check (atom? content) => #t)
      (check (atom-value content)
        =>
        "这是一个注释"
      ) ;check
    ) ;let
  ) ;let
) ;let

;; 测试 scan-string：空注释
(let ((results (scan-string "(*comment* \"\")")
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let ((first (vector-ref results 0)))
    (check (env-tag-name first)
      =>
      "*comment*"
    ) ;check
    (let ((content (vector-ref (env-children first) 0)
          ) ;content
         ) ;
      (check (atom-value content) => "")
    ) ;let
  ) ;let
) ;let

;; 测试 scan-string：多个注释
(let ((results (scan-string "(*comment* \"注释1\") (*comment* \"注释2\")"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 2)
  (check (env-tag-name (vector-ref results 0))
    =>
    "*comment*"
  ) ;check
  (check (env-tag-name (vector-ref results 1))
    =>
    "*comment*"
  ) ;check
) ;let

;; 测试 scan-string：注释和普通代码混合
(let ((results (scan-string "(define x 1)\n(*comment* \"这是注释\")\n(define y 2)"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 3)
  ;; 第一个是 define
  (check (env-tag-name (vector-ref results 0))
    =>
    "define"
  ) ;check
  ;; 第二个是注释
  (check (env-tag-name (vector-ref results 1))
    =>
    "*comment*"
  ) ;check
  ;; 第三个是 define
  (check (env-tag-name (vector-ref results 2))
    =>
    "define"
  ) ;check
) ;let

;; 测试 scan-string：注释中包含特殊字符
(let ((results (scan-string "(*comment* \"包含 (括号) 和 [方括号] 的注释\")"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let ((first (vector-ref results 0)))
    (check (env-tag-name first)
      =>
      "*comment*"
    ) ;check
    (let ((content (vector-ref (env-children first) 0)
          ) ;content
         ) ;
      (check (atom-value content)
        =>
        "包含 (括号) 和 [方括号] 的注释"
      ) ;check
    ) ;let
  ) ;let
) ;let

;; 测试 scan-string：点对表达式 '(a b . c)
;; '(a b . c) 读取为 (#_quote (a b . c))
;; quote 形式整个作为一个 env，没有 children
(let ((results (scan-string #"CODE"'(a b . c)"CODE")
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  ;; 顶层是 quote env
  (let ((quote-env (vector-ref results 0)))
    (check (env? quote-env) => #t)
    (check (env-tag-name quote-env)
      =>
      "#_quote"
    ) ;check
    (check (env-depth quote-env) => 0)
    ;; quote 形式没有 children
    (check (vector-length (env-children quote-env))
      =>
      0
    ) ;check
    ;; 原始值保留在 value 字段
    (check (env-value quote-env)
      =>
      '(#_quote (a b . c))
    ) ;check
  ) ;let
) ;let

;; 测试 scan-string：'(quote define) 这种嵌套 quote 的情况
;; '(quote define) 读取为 (#_quote (quote define))
;; quote 形式整个作为一个 env，没有 children
(let ((results (scan-string #"CODE"'(quote define)"CODE"
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  ;; 顶层是 quote env
  (let ((quote-env (vector-ref results 0)))
    (check (env? quote-env) => #t)
    (check (env-tag-name quote-env)
      =>
      "#_quote"
    ) ;check
    (check (env-depth quote-env) => 0)
    ;; quote 形式没有 children
    (check (vector-length (env-children quote-env))
      =>
      0
    ) ;check
    ;; 原始值保留在 value 字段
    (check (env-value quote-env)
      =>
      '(#_quote (quote define))
    ) ;check
  ) ;let
) ;let

;; 测试 scan-string：raw string 会保留源码字面量
(let ((results (scan-string "#\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\""
               ) ;scan-string
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let* ((node (vector-ref results 0))
         (literal (atom-value node))
        ) ;
    (check (atom? node) => #t)
    (check (raw-string-literal? literal)
      =>
      #t
    ) ;check
    (check (raw-string-literal-source literal)
      =>
      "#\"SQL\"\n  ;; not a comment\n  SELECT 1\n  \"SQL\""
    ) ;check
    (check (raw-string-literal-value literal)
      =>
      "\n  ;; not a comment\n  SELECT 1\n  "
    ) ;check
  ) ;let*
) ;let

;; bare syntax object 应该被当作 atom，而不是 pair
(let ((results (scan-string "#_quote")))
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let ((first (vector-ref results 0)))
    (check (atom? first) => #t)
    (check (syntax? (atom-value first))
      =>
      #t
    ) ;check
    (check (object->string (atom-value first) #f)
      =>
      "#_quote"
    ) ;check
  ) ;let
) ;let

;; quasiquote 的内部形式应该被正规化为 reader 语义
(let ((results (scan-string #"CODE"`(a ,@rest)"CODE")
      ) ;results
     ) ;
  (check (vector? results) => #t)
  (check (vector-length results) => 1)
  (let ((root (vector-ref results 0)))
    (check (env? root) => #t)
    (check (env-tag-name root)
      =>
      "quasiquote"
    ) ;check
    (check (env-value root) => '(quasiquote (a (unquote-splicing rest))))
  ) ;let
) ;let

(check-report)
