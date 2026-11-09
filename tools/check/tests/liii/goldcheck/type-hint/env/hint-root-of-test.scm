;; hint-root-of
;; 从 hint 文件路径推导其所在的本地 hint 根目录。
;;
;; 语法
;; ----
;; (hint-root-of hint-file)
;;
;; 参数
;; ----
;; hint-file : string  hint 库文件路径
;;
;; 返回值
;; ------
;; string 或 #f
;; 本地 hint 根目录（如 "goldfish/hint"）；路径无 hint 段时返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint env))

(check-set-mode! 'report-failed)

;; 1. 各板块 hint 目录
(check (hint-root-of "goldfish/hint/liii/clean.scm") => "goldfish/hint")
(check (hint-root-of "r7rs/hint/scheme/base.scm") => "r7rs/hint")
(check (hint-root-of "library/hint/liii/json.scm") => "library/hint")
(check (hint-root-of "tools/check/hint/liii/goldcheck/path.scm") => "tools/check/hint")

;; 2. 无 hint 段返回 #f
(check (hint-root-of "goldfish/liii/string.scm") => #f)

(check-report)
