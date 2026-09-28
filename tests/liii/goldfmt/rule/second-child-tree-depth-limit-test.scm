(import (liii check)
  (liii goldfmt rule)
) ;import

(check-set-mode! 'report-failed)

;; second-child-tree-depth-limit
;; 读取某个 tag 在第二子节点树深度达到多少时触发整体换行/禁止 inline。
;;
;; 语法
;; ----
;; (second-child-tree-depth-limit tag-name)
;;
;; 参数
;; ----
;; tag-name : string?
;; env 的 tag-name。
;;
;; 返回值
;; ------
;; integer?
;; 允许第二子节点保留在第一行的最大树深度阈值（>= 该值时整体换行）。
;;
;; 说明
;; ----
;; 该规则来自 `node-rules.json` 的 secondChildTreeDepthLimit 字段。
;; 未显式配置时默认为 4；如 select 配置为 3。

(check (second-child-tree-depth-limit "select") => 3)
(check (second-child-tree-depth-limit "begin") => 4)
(check (second-child-tree-depth-limit "unknown-tag") => 4)

(check-report)
