(import (scheme base)
        (liii check)
        (liii list)
        (liii string)
        (liii tui)
        (liii pi code tui renderers)
        (liii pi code tui views))

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
  ;; 包含标题行（$ git status -s）、两行结果、以及末尾的空行隔断，共 4 行
  (check (length rendered) => 4)
  ;; 标题行包含绿色背景色码和 "$ git status -s"
  (check (string-contains? (car rendered) "48;2;35;55;35") => #t)
  (check (string-contains? (car rendered) "$ git status -s") => #t)
  ;; 第二行结果带有深绿背景并填充空格至宽度 40
  (check (string-contains? (cadr rendered) "48;2;35;55;35") => #t)
  (check (string-contains? (cadr rendered) "M a.scm") => #t)
  ;; 第三行结果
  (check (string-contains? (caddr rendered) "?? b.scm") => #t))

;; 测试 read 失败卡片
(let* ((card (make-tool-card "read" '(("path" . "missing.txt")) 'error "File not found"))
       (w 40)
       (rendered (component-render card w)))
  ;; 出错卡片应包含红色背景
  (check (string-contains? (car rendered) "48;2;60;35;35") => #t)
  (check (string-contains? (car rendered) "read missing.txt") => #t)
  (check (string-contains? (cadr rendered) "File not found") => #t))

(check-report)
