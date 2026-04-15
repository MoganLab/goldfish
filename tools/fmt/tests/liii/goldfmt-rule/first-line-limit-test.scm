(import (liii check)
        (liii goldfmt-rule))

(check-set-mode! 'report-failed)

;; first-line-limit
;; 读取某个 tag 在第一行最多保留的 child 数量。
;;
;; 语法
;; ----
;; (first-line-limit tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ------
;; integer?
;; 第一行最多允许保留的 child 数量。
;;
;; 说明
;; ----
;; 该规则来自 `node-rules.json` 的 firstLineLimit 字段。
;; 例如 begin 的值为 0，表示所有 child 都另起一行。

(check (first-line-limit "begin") => 0)
(check (first-line-limit "do") => 2)
(check (first-line-limit "+") => 1)
(check (first-line-limit "unknown-tag") => 1)

(check-report)
