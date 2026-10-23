(import (liii check)
        (liii list)
        (liii os)
        (liii string)
        (liii tui)
        (liii goldrepl))

;; ------------------------------------------------------------------
;; %repl-word-at：光标左侧的当前词
;; ------------------------------------------------------------------
(check (%repl-word-at "string-ap" 9) => "string-ap")
(check (%repl-word-at "(string-ap" 10) => "string-ap")
(check (%repl-word-at "(map string-" 13) => "string-")
(check (%repl-word-at "hello world" 5) => "hello")
(check (%repl-word-at "hello world" 11) => "world")
(check (%repl-word-at "(+ 1 2)" 0) => "")
(check (%repl-word-at "(+ 1 2)" 4) => "")
(check (%repl-word-at "" 0) => "")
;; 光标位置超出文本长度时按文本末尾处理
(check (%repl-word-at "abc" 100) => "abc")

;; ------------------------------------------------------------------
;; %repl-completions：前缀补全
;; ------------------------------------------------------------------
(%repl-update-symbol-cache!)

(let ((cands (%repl-completions "string-ap")))
  (check-true (pair? cands))
  (check-true (pair? (filter (lambda (e) (string=? (car e) "string-append")) cands))))

(check (%repl-completions "no-such-symbol-xyz-abc") => '())

;; 全部补全候选都以给定前缀开头
(let ((cands (%repl-completions "map")))
  (check-true (pair? cands))
  (check-true (every (lambda (e) (string-starts? (car e) "map")) cands)))

;; ------------------------------------------------------------------
;; %repl-history-file-lines：历史文件解析（isocline 格式）
;; ------------------------------------------------------------------
(let* ((tmp (string-append "/tmp/goldrepl-history-test-" (number->string (getpid)) ".txt"))
       (p (open-output-file tmp)))
  (display "#!isocline 3\n" p)
  (display "(+ 1 2)\n" p)
  (display "(display \"hello\")\\n(next line)\n" p)
  (display "foo\\\\bar\n" p)
  (close-output-port p)
  (let ((res (%repl-history-file-lines tmp)))
    ;; count 为条目数（不含 #! 头）
    (check (car res) => 3)
    (check (length (cdr res)) => 3)
    (check (car (cdr res)) => "(+ 1 2)")
    ;; 转义保持原文（未反转义）
    (check (cadr (cdr res)) => "(display \"hello\")\\n(next line)")
    (check (caddr (cdr res)) => "foo\\\\bar"))
  (remove tmp))

;; 文件不存在
(let ((res (%repl-history-file-lines "/tmp/goldrepl-no-such-file-xyz.txt")))
  (check (car res) => 0)
  (check (cdr res) => '()))

;; 无 #! 头部时逐条计数
(let* ((tmp (string-append "/tmp/goldrepl-history-test2-" (number->string (getpid)) ".txt"))
       (p (open-output-file tmp)))
  (display "(+ 1 2)\n" p)
  (display "abc\n" p)
  (close-output-port p)
  (let ((res (%repl-history-file-lines tmp)))
    (check (car res) => 2)
    (check (length (cdr res)) => 2))
  (remove tmp))

;; ------------------------------------------------------------------
;; %repl-styled：ANSI 样式包裹
;; ------------------------------------------------------------------
(check (%repl-styled "error" "oops") => "\x1b;[31moops\x1b;[0m")
(check (%repl-styled "prompt" "gf> ") => "\x1b;[33mgf> \x1b;[0m")
(check (%repl-styled "no-such-style" "x") => "x")

;; ------------------------------------------------------------------
;; %repl-buffer / %repl-buffer-set!
;; ------------------------------------------------------------------
(let ((buf (%repl-buffer)))
  (check (car buf) => "")
  (%repl-buffer-set! buf '("(define (f x)" "  (+ x 1))"))
  (check (car buf) => '("(define (f x)" "  (+ x 1))")))

;; ------------------------------------------------------------------
;; 括号配对测试：%repl-missing-parens
;; ------------------------------------------------------------------
(check (%repl-missing-parens "(+ 1 2)") => 0)
(check (%repl-missing-parens "(define (f x)") => 1)
(check (%repl-missing-parens "(define (f x) (let ((y 1))") => 2)
(check (%repl-missing-parens ")") => -1)
(check (%repl-missing-parens "(string-append \")\" \"(\")") => 0)
(check (%repl-missing-parens "(+ 1 2) ; unclosed parenthesis: (") => 0)

;; ------------------------------------------------------------------
;; 历史记录条目转义与反转义测试
;; ------------------------------------------------------------------
(check (%repl-escape-history-entry "foo\nbar") => "foo\\nbar")
(check (%repl-escape-history-entry "foo\\bar") => "foo\\\\bar")
(check (%repl-unescape-history-entry "foo\\nbar") => "foo\nbar")
(check (%repl-unescape-history-entry "foo\\\\bar") => "foo\\bar")
(check (%repl-unescape-history-entry (%repl-escape-history-entry "a\\b\nc")) => "a\\b\nc")

;; ------------------------------------------------------------------
;; %repl-eval：求值、输出捕获与返回值判定（包含 #f 与异常）
;; ------------------------------------------------------------------
;; 1. 正常求值
(let ((res (%repl-eval "(+ 1 2)")))
  (check (car res) => "")       ;; out
  (check (cadr res) => "")      ;; err
  (check (caddr res) => #t)     ;; ok?
  (check (cadddr res) => 3))    ;; result

;; 2. 正常求值返回 #f（重点：ok? 必须为 #t，不能误判为失败）
(let ((res (%repl-eval "(= 1 2)")))
  (check (car res) => "")
  (check (cadr res) => "")
  (check (caddr res) => #t)
  (check (cadddr res) => #f))

;; 3. 正常求值包含标准输出
(let ((res (%repl-eval "(display \"hello\")")))
  (check (car res) => "hello")
  (check (caddr res) => #t))

;; 4. 运行时异常（ok? 必须为 #f，err 中包含错误信息）
(let ((res (%repl-eval "(/ 1 0)")))
  (check (caddr res) => #f)
  (check-true (> (string-length (cadr res)) 0)))

;; 5. 顶层持久化求值
(%repl-eval "(define *goldrepl-test-var* 42)")
(let ((res (%repl-eval "*goldrepl-test-var*")))
  (check (caddr res) => #t)
  (check (cadddr res) => 42))

;; ------------------------------------------------------------------
;; 元命令测试：%repl-meta-describe
;; ------------------------------------------------------------------
(let ((lines (%repl-meta-describe "car")))
  (check-true (pair? lines))
  ;; 包含 procedure 类型
  (check-true (any (lambda (l) (string-contains l "procedure")) lines))
  ;; 包含 Arity 信息
  (check-true (any (lambda (l) (string-contains l "Arity")) lines)))

;; ------------------------------------------------------------------
;; 历史文件路径测试：%repl-history-file
;; ------------------------------------------------------------------
(let ((path (%repl-history-file)))
  (check-true (string? path))
  (check-true (string-ends? path "/history")))

;; ------------------------------------------------------------------
;; 语法高亮行测试：%repl-highlight-line
;; ------------------------------------------------------------------
(check-true (string? (%repl-highlight-line "(define (f x) (+ x 1))")))
(check-true (string? (%repl-highlight-line "; comment text")))
(check-true (string-contains (%repl-highlight-line "; comment") "\x1b;[90m"))
(check-true (string-contains (%repl-highlight-line "(define x 100)") "\x1b;[35mdefine\x1b;[0m"))
(check-true (string-contains (%repl-highlight-line "(define x 100)") "\x1b;[33m100\x1b;[0m"))

;; ------------------------------------------------------------------
;; 会话值注册测试：%repl-register-value!
;; ------------------------------------------------------------------
(let ((sym (%repl-register-value! 123)))
  (check-true (symbol? sym))
  (check-true (string-starts? (symbol->string sym) "$")))

;; ------------------------------------------------------------------
;; 边界求值测试：多值与空值
;; ------------------------------------------------------------------
(let ((res (%repl-eval "(values 1 2 3)")))
  (check (caddr res) => #t)
  (check (cadddr res) => '(1 2 3)))

(let ((res (%repl-eval "(values)")))
  (check (caddr res) => #t)
  (check (eq? (cadddr res) (if #f #f)) => #t)
  (let ((sym (%repl-register-value! (cadddr res))))
    (check-true (symbol? sym))))

(let ((res (%repl-eval "'()")))
  (check (caddr res) => #t)
  (check (cadddr res) => '()))

;; ------------------------------------------------------------------
;; 元命令完整性测试
;; ------------------------------------------------------------------
;; 未知元命令
(let-values (((quit? lines) (%repl-handle-meta ",xyz")))
  (check quit? => #f)
  (check-true (string-contains (car lines) "Unknown meta command")))

;; ,apropos 搜索
(let ((lines (%repl-meta-apropos "pi")))
  (check-true (pair? lines)))

;; ,describe 描述数值
(let ((lines (%repl-meta-describe "*goldrepl-test-var*")))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "Value:")) lines)))

;; ------------------------------------------------------------------
;; 补全单候选与多候选逻辑深度测试
;; ------------------------------------------------------------------
;; 单候选：string-app -> string-append
(let* ((ed (make-text-input "string-app"))
       (cands (%repl-completions "string-app"))
       (app-out! (lambda (l) #f)))
  (let-values (((rest-cands idx ok?)
                (%repl-try-complete-first! ed "string-app" '(0 10) "string-app" cands app-out!)))
    (check ok? => #t)
    (check (editor-text ed) => "string-append")))

;; ------------------------------------------------------------------
;; 补全无候选时不崩溃测试：%repl-try-complete-first!
;; ------------------------------------------------------------------
(let* ((ed (make-text-input "dd"))
       (app-out! (lambda (lines) #f)))
  (let-values (((cands idx ok?)
                (%repl-try-complete-first! ed "dd" '(0 2) "dd" '() app-out!)))
    (check cands => '())
    (check idx => 0)
    (check ok? => #f)))

;; ------------------------------------------------------------------
;; F1 编辑帮助测试：%repl-editor-help-lines
;; ------------------------------------------------------------------
(let ((lines (%repl-editor-help-lines)))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "Editing Commands")) lines))
  (check-true (any (lambda (l) (string-contains l "shift-tab")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-a")) lines))
  (check-true (any (lambda (l) (string-contains l "ctrl-k")) lines)))

(check-report)
