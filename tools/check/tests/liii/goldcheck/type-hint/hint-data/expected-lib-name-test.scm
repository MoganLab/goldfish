;; expected-lib-name
;; 根据 hint 库文件路径推导其预期的 Scheme 库名符号列表。
;;
;; 语法
;; ----
;; (expected-lib-name hint-path)
;;
;; 参数
;; ----
;; hint-path : string   类型提示库文件路径
;;
;; 返回值
;; ------
;; list of symbol 或 #f
;; 预期库名符号列表（如 '(hint scheme base)），路径不含 hint 段时返回 #f。

(import (scheme base) (liii check) (liii goldcheck type-hint hint-data))

(check-set-mode! 'report-failed)

;; 1. 包含 hint 目录段的路径推导
(check (expected-lib-name "r7rs/hint/scheme/base.scm")
  =>
  '(hint scheme base)
) ;check
(check (expected-lib-name "tools/check/hint/liii/clean.scm")
  =>
  '(hint liii clean)
) ;check

;; 2. 无 hint 目录段返回 #f
(check (expected-lib-name "r7rs/scheme/base.scm") => #f)

(check-report)
