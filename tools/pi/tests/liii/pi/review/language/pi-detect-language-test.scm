(import (liii check)
        (liii pi review language)
) ;import

(check-set-mode! 'report-failed)

;; pi-detect-language
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
