(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; allow-first-line-child-env?
;; 判断某个 tag 是否允许第一行出现直接子 env。
;;
;; 语法
;; ----
;; (allow-first-line-child-env? tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ------
;; boolean?
;; 返回 #t 表示第一行可以保留子 env，返回 #f 表示遇到子 env 时需要换行。
;;
;; 说明
;; ----
;; 该规则来自 `node-rules.json` 的 allowFirstLineChildEnv 字段。
;; 未配置的 tag 会使用 default 规则。

(check (allow-first-line-child-env? "quote") => #f)
(check (allow-first-line-child-env? "define") => #t)
(check (allow-first-line-child-env? "unknown-tag") => #t)

(check-report)
