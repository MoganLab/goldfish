(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; never-inline-when-first-child-env?
;; 判断某个 tag 是否在第一个 child 是 env 时禁止单行输出。
;;
;; 语法
;; ----
;; (never-inline-when-first-child-env? tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示当第一个 child 是 env 时，该 tag 需要跨行输出。
;;
;; 说明
;; ----
;; 该规则来自 `node-rules.json` 的 neverInlineWhenFirstChildEnv 字段。
;; 典型例子是函数形式的 define：`(define (f x) body)`。

(check (never-inline-when-first-child-env? "define") => #t)
(check (never-inline-when-first-child-env? "define-values") => #t)
(check (never-inline-when-first-child-env? "+") => #f)
(check (never-inline-when-first-child-env? "unknown-tag") => #f)

(check-report)
