(import (liii check)
  (liii list)
  (liii os)
  (liii string)
  (liii tui)
  (liii goldrepl)
) ;import

;; %repl-word-at：光标左侧的当前词
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

;; %repl-completions：前缀补全
(%repl-update-symbol-cache!)

(let ((cands (%repl-completions "string-ap")))
  (check-true (pair? cands))
  (check-true
    (pair?
      (filter (lambda (e) (string=? (car e) "string-append")) cands)
    ) ;pair?
  ) ;check-true
) ;let

(check (%repl-completions "no-such-symbol-xyz-abc") => '())

;; 全部补全候选都以给定前缀开头
(let ((cands (%repl-completions "map")))
  (check-true (pair? cands))
  (check-true
    (every (lambda (e) (string-starts? (car e) "map")) cands)
  ) ;check-true
) ;let

(define (%test-temp-path name)
  (let ((tmp-dir
          (cond ((getenv "TEMP") => (lambda (p) (string-replace p "\\" "/")))
                ((getenv "TMP") => (lambda (p) (string-replace p "\\" "/")))
                (else "/tmp")
          ) ;cond
        ) ;tmp-dir
       ) ;
    (string-append tmp-dir "/" name)
  ) ;let
) ;define

;; %repl-history-file-lines：历史文件解析（isocline 格式）
(let* ((tmp
         (%test-temp-path (string-append "goldrepl-history-test-" (number->string (getpid)) ".txt")
         ) ;%test-temp-path
       ) ;tmp
       (p (open-output-file tmp))
      ) ;
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
    (check (caddr (cdr res)) => "foo\\\\bar")
  ) ;let
  (remove tmp)
) ;let*

;; 文件不存在
(let ((res (%repl-history-file-lines (%test-temp-path "goldrepl-no-such-file-xyz.txt"))
      ) ;res
     ) ;
  (check (car res) => 0)
  (check (cdr res) => '())
) ;let

;; 无 #! 头部时逐条计数
(let* ((tmp
         (%test-temp-path (string-append "goldrepl-history-test2-" (number->string (getpid)) ".txt")
         ) ;%test-temp-path
       ) ;tmp
       (p (open-output-file tmp))
      ) ;
  (display "(+ 1 2)\n" p)
  (display "abc\n" p)
  (close-output-port p)
  (let ((res (%repl-history-file-lines tmp)))
    (check (car res) => 2)
    (check (length (cdr res)) => 2)
  ) ;let
  (remove tmp)
) ;let*

;; %repl-styled：ANSI 样式包裹
(check (%repl-styled "error" "oops") => "\x1b;[31moops\x1b;[0m")
(check (%repl-styled "prompt" "gf> ") => "\x1b;[33mgf> \x1b;[0m")
(check (%repl-styled "no-such-style" "x") => "x")

;; %repl-buffer / %repl-buffer-set!
(let ((buf (%repl-buffer)))
  (check (car buf) => "")
  (%repl-buffer-set! buf '("(define (f x)" "  (+ x 1))"))
  (check (car buf) => '("(define (f x)" "  (+ x 1))"))
) ;let

;; 括号配对测试：%repl-missing-parens
(check (%repl-missing-parens "(+ 1 2)") => 0)
(check (%repl-missing-parens "(define (f x)") => 1)
(check (%repl-missing-parens "(define (f x) (let ((y 1))") => 2)
(check (%repl-missing-parens ")") => -1)
(check (%repl-missing-parens "(string-append \")\" \"(\")") => 0)
(check (%repl-missing-parens "(+ 1 2) ; unclosed parenthesis: (") => 0)

;; 历史记录条目转义与反转义测试
(check (%repl-escape-history-entry "foo\nbar") => "foo\\nbar")
(check (%repl-escape-history-entry "foo\\bar") => "foo\\\\bar")
(check (%repl-unescape-history-entry "foo\\nbar") => "foo\nbar")
(check (%repl-unescape-history-entry "foo\\\\bar") => "foo\\bar")
(check (%repl-unescape-history-entry (%repl-escape-history-entry "a\\b\nc"))
  =>
  "a\\b\nc"
) ;check

;; %repl-eval：求值、输出捕获与返回值判定（包含 #f 与异常）
;; 1. 正常求值
(let ((res (%repl-eval "(+ 1 2)")))
  (check (car res) => "")
  ;; out
  (check (cadr res) => "")
  ;; err
  (check (caddr res) => #t)
  ;; ok?
  (check (cadddr res) => 3)
) ;let
;; result

;; 2. 正常求值返回 #f（重点：ok? 必须为 #t，不能误判为失败）
(let ((res (%repl-eval "(= 1 2)")))
  (check (car res) => "")
  (check (cadr res) => "")
  (check (caddr res) => #t)
  (check (cadddr res) => #f)
) ;let

;; 3. 正常求值包含标准输出
(let ((res (%repl-eval "(display \"hello\")")))
  (check (car res) => "hello")
  (check (caddr res) => #t)
) ;let

;; 4. 运行时异常（ok? 必须为 #f，err 中包含错误信息）
(let ((res (%repl-eval "(/ 1 0)")))
  (check (caddr res) => #f)
  (check-true (> (string-length (cadr res)) 0))
) ;let

;; 5. 顶层持久化求值
(%repl-eval "(define *goldrepl-test-var* 42)")
(let ((res (%repl-eval "*goldrepl-test-var*")))
  (check (caddr res) => #t)
  (check (cadddr res) => 42)
) ;let

;; 元命令测试：%repl-meta-describe
(let ((lines (%repl-meta-describe "car")))
  (check-true (pair? lines))
  ;; 包含 procedure 类型
  (check-true (any (lambda (l) (string-contains l "procedure")) lines))
  ;; 包含 Arity 信息
  (check-true (any (lambda (l) (string-contains l "Arity")) lines))
) ;let

;; 历史文件路径测试：%repl-history-file
(let ((path (%repl-history-file)))
  (check-true (string? path))
  (check-true (string-ends? path "/history"))
) ;let

;; 语法高亮行测试：%repl-highlight-line
(check-true (string? (%repl-highlight-line "(define (f x) (+ x 1))")))
(check-true (string? (%repl-highlight-line "; comment text")))
(check-true (string-contains (%repl-highlight-line "; comment") "\x1b;[90m"))
(check-true (string-contains (%repl-highlight-line "(define x 100)")
              "\x1b;[35mdefine\x1b;[0m"
            ) ;string-contains
) ;check-true
(check-true (string-contains (%repl-highlight-line "(define x 100)") "\x1b;[33m100\x1b;[0m")
) ;check-true

;; 语法高亮行中文测试：包含 UTF-8 多字节字符（如“你好”、“中国”）的高亮不能破坏 UTF-8 编码
(let ((hl (%repl-highlight-line "(define 你好 100)")))
  (check-true (string-contains hl "\x1b;[35mdefine\x1b;[0m"))
  (check-true (string-contains hl "你好"))
) ;let
(let ((hl (%repl-highlight-line "(display \"你好世界\")")))
  (check-true (string-contains hl "你好世界"))
) ;let

;; 会话值注册测试：%repl-register-value!
(let ((sym (%repl-register-value! 123)))
  (check-true (symbol? sym))
  (check-true (string-starts? (symbol->string sym) "$"))
) ;let

;; 边界求值测试：多值与空值
(let ((res (%repl-eval "(values 1 2 3)")))
  (check (caddr res) => #t)
  (check (cadddr res) => '(1 2 3))
) ;let

(let ((res (%repl-eval "(values)")))
  (check (caddr res) => #t)
  (check (eq? (cadddr res) (if #f #f)) => #t)
  (let ((sym (%repl-register-value! (cadddr res))))
    (check-true (symbol? sym))
  ) ;let
) ;let

(let ((res (%repl-eval "'()")))
  (check (caddr res) => #t)
  (check (cadddr res) => '())
) ;let

;; 元命令完整性测试
;; 未知元命令
(let-values (((quit? lines) (%repl-handle-meta ",xyz")))
  (check quit? => #f)
  (check-true (string-contains (car lines) "Unknown meta command"))
) ;let-values

;; ,apropos 搜索
(let ((lines (%repl-meta-apropos "pi")))
  (check-true (pair? lines))
) ;let

;; ,describe 描述数值
(let ((lines (%repl-meta-describe "*goldrepl-test-var*")))
  (check-true (pair? lines))
  (check-true (any (lambda (l) (string-contains l "Value:")) lines))
) ;let

(check-report)
