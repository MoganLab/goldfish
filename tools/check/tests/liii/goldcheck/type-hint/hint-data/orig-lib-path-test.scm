;; orig-lib-path
;; 根据 hint 库路径推导其对应的原库文件路径（去除路径中的 hint 目录段）。
;;
;; 原库可能位于 R7RS 板块 `r7rs/` 、语言层 `goldfish/` 或标准库层 `library/`，
;; 因此依次尝试 hint 同层路径、各板块路径，返回首个存在的文件；均不存在时
;; 回退到 hint 同层路径。
;;
;; 语法
;; ----
;; (orig-lib-path hint-path)
;;
;; 参数
;; ----
;; hint-path : string   类型提示库文件路径
;;
;; 返回值
;; ------
;; string 或 #f
;; 原库相对路径，不含 hint 目录段时返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 成功推导原库路径
(check (orig-lib-path "r7rs/hint/scheme/base.scm")
  =>
  "r7rs/scheme/base.scm"
) ;check
(check (orig-lib-path "goldfish/hint/liii/gfproject/config.scm")
  =>
  "library/liii/gfproject/config.scm"
) ;check
(check (orig-lib-path "tools/check/hint/liii/clean.scm")
  =>
  "tools/check/liii/clean.scm"
) ;check

;; 2. 无 hint 段返回 #f
(check (orig-lib-path "r7rs/scheme/base.scm") => #f)

(check-report)
