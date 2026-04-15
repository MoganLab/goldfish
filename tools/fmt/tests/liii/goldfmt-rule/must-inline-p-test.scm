(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; must-inline?
;; 判断某个 tag 是否必须单行输出。
;;
;; 语法
;; ----
;; (must-inline? tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示该 tag 必须 inline。
;;
;; 说明
;; ----
;; 该规则来自 `node-rules.json` 的 mustInline 字段。
;; `can-inline?` 会优先使用这个规则允许 inline。

(check (must-inline? "quote") => #f)
(check (must-inline? "+") => #f)
(check (must-inline? "unknown-tag") => #f)

(check-report)
