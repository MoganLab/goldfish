(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; never-inline?
;; 判断某个 tag 是否禁止单行输出。
;;
;; 语法
;; ----
;; (never-inline? tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示该 tag 必须跨行输出。
;;
;; 说明
;; ----
;; 该规则来自 `node-rules.json` 的 neverInline 字段。
;; `can-inline?` 会优先使用这个规则阻止 inline。

(check (never-inline? "begin") => #t)
(check (never-inline? "+") => #f)
(check (never-inline? "unknown-tag") => #f)

(check-report)
