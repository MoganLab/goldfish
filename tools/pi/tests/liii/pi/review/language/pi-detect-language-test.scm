;; pi-detect-language
;; 根据文件路径的扩展名识别编程语言类型。
;;
;; 语法
;; ----
;; (pi-detect-language path)
;;
;; 参数
;; ----
;; path : string?
;; 文件路径字符串。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 识别出的语言标识字符串（如 "typescript"、"python"），无法识别时返回 #f。

(import (liii check)
        (liii pi review language)
) ;import

(check-set-mode! 'report-failed)

;; 按扩展名检测语言，未知扩展名返回 #f。
(check (pi-detect-language "src/app.ts") => "typescript")
(check (pi-detect-language "src/app.tsx") => "typescript")
(check (pi-detect-language "src/app.js") => "typescript")
(check (pi-detect-language "src/app.jsx") => "typescript")
(check (pi-detect-language "scripts/run.py") => "python")
(check (pi-detect-language "cmd/main.go") => "go")
(check (pi-detect-language "src/lib.rs") => "rust")
(check (pi-detect-language "src/Main.java") => "java")
(check (pi-detect-language "web/index.php") => "php")
(check (pi-detect-language "tools/pi/liii/pi/agent.scm") => "scheme")
(check (pi-detect-language "README.md") => #f)
(check (pi-detect-language "Makefile") => #f)
(check (pi-detect-language "src/app.TS") => "typescript")
(check (pi-detect-language "dir.with.dots/file") => #f)

(check-report)
