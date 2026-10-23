(import (liii check))
(import (liii goldfmt scan literal))

(check-set-mode! 'report-failed)

;; needs-rewrite-reader-literals?
;; 判断源码文本是否包含 reader 字面量（raw string 或字符字面量），
;; 从而决定 scan-string 是否需要先做字面量改写。
;;
;; 语法
;; ----
;; (needs-rewrite-reader-literals? str)
;;
;; 参数
;; ----
;; str : string?
;; 源码文本。
;;
;; 返回值
;; ------
;; boolean?
;; 包含 #" 或 #\ 前缀时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 1. 这是一个快速预检，仅做字符级扫描
;; 2. 字符串与注释中的 #\" 也会被判为 #t（保守策略，正确性优先）

;; 测试：包含 raw string 时返回 #t
(check (needs-rewrite-reader-literals? "#\"hello\"") => #t)

;; 测试：包含字符字面量时返回 #t
(check (needs-rewrite-reader-literals? "#\\a") => #t)

;; 测试：普通代码返回 #f
(check (needs-rewrite-reader-literals? "(+ 1 2)") => #f)

;; 测试：空字符串返回 #f
(check (needs-rewrite-reader-literals? "") => #f)

;; 测试：只有 # 结尾时返回 #f
(check (needs-rewrite-reader-literals? "abc #") => #f)

;; 测试：块注释前缀 #| 不触发改写
(check (needs-rewrite-reader-literals? "#| comment |#") => #f)

(check-report)
