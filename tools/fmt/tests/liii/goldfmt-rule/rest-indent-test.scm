(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; rest-indent
;; 读取某个 tag 的后续行子节点缩进策略。
;;
;; 语法
;; ----
;; (rest-indent tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ------
;; symbol?
;; 当前可能返回：
;; 1. 'align-to-first-selected-env
;; 2. 'parent-plus2
;; 3. 'by-first-rest-child
;;
;; 说明
;; ----
;; 该规则来自 `node-rules.json` 的 restIndent 字段。
;; formatter 使用它决定第一行之后的 children 应该如何缩进。

(check (rest-indent "cond") => 'align-to-first-selected-env)
(check (rest-indent "let") => 'parent-plus2)
(check (rest-indent "+") => 'by-first-rest-child)
(check (rest-indent "unknown-tag") => 'by-first-rest-child)

(check-report)
