(import (scheme base)
        (liii time)
        (liii check)
        (liii list)
        (liii string)
        (liii tui)
        (liii pi code modes interactive renderers)
        (liii pi code modes interactive views))

(check-set-mode! 'report-failed)

;; -----------------------------------------------------------------------------
;; 1. format-tool-call: 各内置工具的命令调用行格式化
;; -----------------------------------------------------------------------------

;; bash 工具
(check (format-tool-call "bash" '(("command" . "ls -la")))
       => "$ ls -la")
(check (format-tool-call "bash" '((command . "npm test") (timeout . 30)))
       => "$ npm test (timeout 30s)")
(check (format-tool-call "bash" "echo hello")
       => "$ echo hello")
(check (format-tool-call "bash" '())
       => "$ ...")

;; read 工具
(check (format-tool-call "read" '(("path" . "src/app.scm")))
       => "read src/app.scm")
(check (format-tool-call "read" '(("file_path" . "src/app.scm") ("offset" . 10) ("limit" . 20)))
       => "read src/app.scm [L10-29]")
(check (format-tool-call "read" "main.scm")
       => "read main.scm")

;; write 工具
(check (format-tool-call "write" '(("path" . "README.md") ("content" . "# Hello")))
       => "write README.md")
(check (format-tool-call "write" '(("file_path" . "out.txt")))
       => "write out.txt")
(check (format-tool-call "write" "out.txt")
       => "write out.txt")

;; edit 工具
(check (format-tool-call "edit" '(("path" . "main.scm") ("edits" . ())))
       => "edit main.scm")
(check (format-tool-call "edit" "main.scm")
       => "edit main.scm")

;; grep 工具
(check (format-tool-call "grep" '(("pattern" . "define") ("path" . "src/")))
       => "grep \"define\" src/")
(check (format-tool-call "grep" '(("pattern" . "define")))
       => "grep \"define\"")

;; find 工具
(check (format-tool-call "find" '(("pattern" . "*.scm") ("path" . "tools/")))
       => "find \"*.scm\" tools/")
(check (format-tool-call "find" '(("pattern" . "*.scm")))
       => "find \"*.scm\"")

;; ls 工具
(check (format-tool-call "ls" '(("path" . "src/")))
       => "ls src/")
(check (format-tool-call "ls" '())
       => "ls .")

;; web_search 工具
(check (format-tool-call "web_search" '(("query" . "Goldfish Scheme")))
       => "web_search \"Goldfish Scheme\"")

;; 兜底未知工具
(check (format-tool-call "custom_tool" '(("key" . "value")))
       => "custom_tool key=value")

;; -----------------------------------------------------------------------------
;; 2. format-tool-result-lines: 多行结果切分与折叠预览
;; -----------------------------------------------------------------------------

;; 简单单行
(check (format-tool-result-lines "bash" "Hello World")
       => '("Hello World"))

;; 短多行（<= 5 行完整保留）
(let ((lines (format-tool-result-lines "bash" "line1\nline2\nline3")))
  (check lines => '("line1" "line2" "line3")))

;; bash 超长（如 8 行）：默认保留尾部 5 行，并在首部追加折叠提示
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "bash" text :max-lines 5)))
  (check (length lines) => 6)
  (check (car lines) => "... (3 earlier lines)")
  (check (cdr lines) => '("4" "5" "6" "7" "8")))

;; read/grep 等超长（如 8 行）：默认保留头部 5 行，并在尾部追加折叠提示
(let* ((text "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-tool-result-lines "read" text :max-lines 5)))
  (check (length lines) => 6)
  (check (list-ref lines 5) => "... (3 more lines)")
  (check (take lines 5) => '("1" "2" "3" "4" "5")))

;; 结构化结果（如 pi-tool-result 或 inlet 包含 content）提取测试
(let* ((mock-inlet (inlet 'is-error #f 'content #(((type . "text") (text . "ddd\n")))))
       (lines (format-tool-result-lines "bash" mock-inlet)))
  (check lines => '("ddd")))

;; -----------------------------------------------------------------------------
;; 3. make-tool-card 组件集成测试（验证多行卡片背景色渲染）
;; -----------------------------------------------------------------------------

;; 测试 bash 成功多行输出卡片
(let* ((card (make-tool-card "bash" '(("command" . "git status -s")) 'success " M a.scm\n?? b.scm"))
       (w 40)
       (rendered (component-render card w)))
  (check (component? card) => #t)
  ;; 包含标题行、输出前空行、两行结果、末尾空行隔断，共 5 行
  (check (length rendered) => 5)
  ;; 标题行包含绿色背景色码和 "$ git status -s"
  (check (string-contains? (car rendered) "48;2;35;55;35") => #t)
  (check (string-contains? (car rendered) "$ git status -s") => #t)
  ;; 输出前空行也带有深绿背景并填充至宽度 40
  (check (string-contains? (cadr rendered) "48;2;35;55;35") => #t)
  ;; 第三、四行为结果
  (check (string-contains? (caddr rendered) "M a.scm") => #t)
  (check (string-contains? (cadddr rendered) "?? b.scm") => #t))

;; 测试 read 失败卡片
(let* ((card (make-tool-card "read" '(("path" . "missing.txt")) 'error "File not found"))
       (w 40)
       (rendered (component-render card w)))
  ;; 出错卡片应包含红色背景
  (check (string-contains? (car rendered) "48;2;60;35;35") => #t)
  (check (string-contains? (car rendered) "read missing.txt") => #t)
  (check (string-contains? (cadr rendered) "File not found") => #t))

;; -----------------------------------------------------------------------------
;; 4. Bash 工具卡片：对齐上游的运行计时与视觉行折叠
;; -----------------------------------------------------------------------------

;; 时长格式：不足一分钟保留一位小数，其余与上游 formatToolDuration 一致
(check (format-tool-duration 4200) => "4.2s")
(check (format-tool-duration 61000) => "1m 1s")
(check (format-tool-duration 3661000) => "1h 1m 1s")

;; 运行中显示 Elapsed；结束后使用执行层记录的耗时，之后不再滚动
(let* ((card (make-tool-card "bash" '(("command" . "sleep 5")) 'pending))
       (pending-text (string-join (map strip-ansi (component-render card 80)) "\n")))
  (check-true (string-contains? pending-text "Elapsed "))
  (tool-card-set-complete! card "done" #f 4200)
  (let ((done-text (string-join (map strip-ansi (component-render card 80)) "\n")))
    (check-true (string-contains? done-text "done"))
    (check-true (string-contains? done-text "Took 4.2s"))
    (check-false (string-contains? done-text "Elapsed"))
    (sleep 0.02)
    (check (string-join (map strip-ansi (component-render card 80)) "\n") => done-text)))

;; Bash 折叠保留最后 5 个视觉行，隐藏行提示及 Ctrl+O 文案与上游一致
(let* ((result "1\n2\n3\n4\n5\n6\n7\n8")
       (lines (format-bash-tool-result-lines result 80 #f)))
  (check (car lines) => "... (3 earlier lines, ctrl+o to expand)")
  (check (cdr lines) => '("4" "5" "6" "7" "8"))
  (check (format-bash-tool-result-lines result 80 #t)
    => '("1" "2" "3" "4" "5" "6" "7" "8")))

;; 一个长逻辑行按终端宽度换为多个视觉行，折叠时不能只按换行符计数
(let ((lines (format-bash-tool-result-lines "abcdefghijklmnop" 8 #f)))
  (check (length lines) => 2)
  (check (car lines) => "abcdefgh")
  (check (cadr lines) => "ijklmnop"))

;; 渲染层对齐上游 getTextOutput + Text：剥 ANSI/OSC、删控制字符与 \r，TAB 展开为定宽 3 空格
(let ((lines (format-bash-tool-result-lines "a\tb\tc" 80 #f)))
  (check lines => '("a   b   c")))

;; 残留 ANSI（含 OSC）与 \r 不出现在渲染行中
(let ((lines (format-bash-tool-result-lines "\x1b]0;title\x07;\x1b[32mok\x1b[0m\r\ndone" 80 #f)))
  (check lines => '("ok" "done")))

;; 控制字符被删除
(let ((lines (format-bash-tool-result-lines "a\x01;b" 80 #f)))
  (check lines => '("ab")))

;; 折叠态尾部处理：500 行大输出只显示最后 5 个视觉行（避免每帧全文 wrap 能CPU飕升）
(let* ((big (string-join (map number->string (iota 500 1)) "\n"))
       (lines (format-bash-tool-result-lines big 80 #f)))
  (check (length lines) => 6)
  (check (car lines) => "... (495 earlier lines, ctrl+o to expand)")
  (check (cadr lines) => "496")
  (check (last lines) => "500"))

;; 超宽行 wrap 后折叠：显示最后 5 个视觉行，hidden 计逻辑行数（尾部处理语义）
(let* ((text (string-append "1\n2\n3\n4\n5\n6\n7\n" "abcdefghijklmnop"))
       (lines (format-bash-tool-result-lines text 8 #f)))
  ;; 尾部 5 逻辑行 = 4..7+超宽行，超宽行 wrap 成 2 视觉行 → M=6 > 5，取最后 5
  (check lines => '("... (3 earlier lines, ctrl+o to expand)"
                   "5" "6" "7" "abcdefgh" "ijklmnop")))

;; 卡片展开状态可往返切换，运行中的日志也遵循同一折叠状态
(let ((card (make-tool-card "bash" '(("command" . "seq 1 8")) 'pending "1\n2\n3\n4\n5\n6\n7\n8")))
  (check-false (tool-card-expanded? card))
  (tool-card-set-expanded! card #t)
  (check-true (tool-card-expanded? card))
  (let ((lines (map string-trim-both (map strip-ansi (component-render card 80)))))
    (check-true (pair? (member "1" lines)))
    (check-true (pair? (member "8" lines))))
  (tool-card-set-expanded! card #f)
  (let ((text (string-join (map strip-ansi (component-render card 80)) "\n")))
    (check-true (string-contains? text "... (3 earlier lines, ctrl+o to expand)"))))

(check-report)
