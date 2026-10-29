(import (scheme base)
  (liii check)
  (liii pi code modes interactive renderers))

(check-set-mode! 'report-failed)

;; 各内置工具的命令调用行格式化
(check (format-tool-call "bash" '(("command" . "ls -la"))) => "$ ls -la")
(check (format-tool-call "bash" '((command . "npm test") (timeout . 30))) => "$ npm test (timeout 30s)")
(check (format-tool-call "bash" "echo hello") => "$ echo hello")
(check (format-tool-call "bash" '()) => "$ ...")

;; 范围后缀对齐上游 formatReadLineRange："路径:起-止"
(check (format-tool-call "read" '(("path" . "src/app.scm"))) => "read src/app.scm")
(check (format-tool-call "read" '(("file_path" . "src/app.scm") ("offset" . 10) ("limit" . 20)))
  => "read src/app.scm:10-29")
(check (format-tool-call "read" '(("path" . "a.scm") ("offset" . 10))) => "read a.scm:10")
;; 实数 offset/limit：展示时取整，避免出现 180.0-299.0 这类浮点范围
(check (format-tool-call "read" '(("path" . "a.scm") ("offset" . 180.0) ("limit" . 120.0)))
  => "read a.scm:180-299")
(check (format-tool-call "read" '(("path" . "a.scm") ("offset" . 180.0))) => "read a.scm:180")
(check (format-tool-call "read" "main.scm") => "read main.scm")

(check (format-tool-call "write" '(("path" . "README.md") ("content" . "# Hello"))) => "write README.md")
(check (format-tool-call "write" '(("file_path" . "out.txt"))) => "write out.txt")
(check (format-tool-call "write" "out.txt") => "write out.txt")

(check (format-tool-call "edit" '(("path" . "main.scm") ("edits" . ()))) => "edit main.scm")
(check (format-tool-call "edit" "main.scm") => "edit main.scm")

(check (format-tool-call "grep" '(("pattern" . "define") ("path" . "src/"))) => "grep /define/ in src/")
(check (format-tool-call "grep" '(("pattern" . "define"))) => "grep /define/ in .")

(check (format-tool-call "find" '(("pattern" . "*.scm") ("path" . "tools/"))) => "find *.scm in tools/")
(check (format-tool-call "find" '(("pattern" . "*.scm"))) => "find *.scm in .")

(check (format-tool-call "ls" '(("path" . "src/"))) => "ls src/")
(check (format-tool-call "ls" '()) => "ls .")

(check (format-tool-call "web_search" '(("query" . "Goldfish Scheme")))
  => "web_search \"Goldfish Scheme\"")

(check (format-tool-call "custom_tool" '(("key" . "value"))) => "custom_tool key=value")

(check-report)
