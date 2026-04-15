(import (liii check)
        (liii string)
        (liii goldfmt-tokenize))

(check-set-mode! 'report-failed)

;; 测试 tokens->string：空列表
(let ((str (tokens->string '())))
  (check (string? str) => #t)
  (check (string-null? str) => #t))

;; 测试 tokens->string：单个代码 token
(let ((str (tokens->string '((code . "(define x 1)")))))
  (check (string? str) => #t)
  (check str => "(define x 1)"))

;; 测试 tokens->string：单个注释 token
(let ((str (tokens->string '((comment . "这是一个注释")))))
  (check (string? str) => #t)
  (check str => "(*comment* \"这是一个注释\")"))

;; 测试 tokens->string：代码和注释混合
(let ((str (tokens->string '((code . "(define x 1)")
                             (comment . "变量定义")
                             (code . "(define y 2)")))))
  (check (string? str) => #t)
  (check str => "(define x 1)\n(*comment* \"变量定义\")\n(define y 2)"))

;; 测试 tokens->string：多个注释
(let ((str (tokens->string '((comment . "注释1")
                             (comment . "注释2")))))
  (check (string? str) => #t)
  (check str => "(*comment* \"注释1\")\n(*comment* \"注释2\")"))

;; 测试 tokens->string：注释内容包含特殊字符
(let ((str (tokens->string '((comment . "包含 (括号) 和 [方括号]")))))
  (check (string? str) => #t)
  (check str => "(*comment* \"包含 (括号) 和 [方括号]\")"))

;; 测试 tokens->string：注释包含双引号（需要转义）
(let ((str (tokens->string '((comment . "包含\"引号\"")))))
  (check (string? str) => #t)
  (check str => "(*comment* \"包含\\\"引号\\\"\")"))

;; 测试 tokens->string：注释包含反斜杠
(let ((str (tokens->string '((comment . "包含\\反斜杠")))))
  (check (string? str) => #t)
  (check str => "(*comment* \"包含\\\\反斜杠\")"))

;; 测试 tokens->string：注释包含换行
(let ((str (tokens->string '((comment . "第一行\n第二行")))))
  (check (string? str) => #t)
  (check str => "(*comment* \"第一行\\n第二行\")"))

;; 测试 tokens->string：空注释
(let ((str (tokens->string '((comment . "")))))
  (check (string? str) => #t)
  (check str => "(*comment* \"\")"))

;; 测试 tokens->string：空行 token
(let ((str (tokens->string '((code . "(define x 1)")
                             (newline . 1)
                             (code . "(define y 2)")))))
  (check (string? str) => #t)
  (check str => "(define x 1)\n(*newline* 1)\n(define y 2)"))

;; 测试 tokens->string：多个空行
(let ((str (tokens->string '((code . "(define x 1)")
                             (newline . 2)
                             (code . "(define y 2)")))))
  (check (string? str) => #t)
  (check str => "(define x 1)\n(*newline* 2)\n(define y 2)"))

;; 测试 tokens->string：代码、注释、空行混合
(let ((str (tokens->string '((code . "(define x 1)")
                             (newline . 1)
                             (comment . "注释")
                             (newline . 2)
                             (code . "(define y 2)")))))
  (check (string? str) => #t)
  (check str => "(define x 1)\n(*newline* 1)\n(*comment* \"注释\")\n(*newline* 2)\n(define y 2)"))

(check-report)
