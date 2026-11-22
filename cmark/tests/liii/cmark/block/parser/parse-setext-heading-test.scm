;; parse-setext-heading
;; 解析 Setext 风格标题。
;;
;; 语法
;; ----
;; (parse-setext-heading lines)
;;
;;
;; 参数
;; ----
;; lines : any
;; 输入参数。
;;
;; 返回值
;; ----
;; cmark-node 或 pair 或 #f
;; 返回解析得到的节点或结构。
;; 描述
;; ----
;; setext-underline? 与 setext-heading-level 用于识别并确定标题级别。

(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (setext-underline? "==="))
(check-true (setext-underline? "---"))
(check-true (setext-underline? "=== "))
(check-true (setext-underline? "  ==="))
(check-false (setext-underline? "    ==="))
(check (setext-heading-level "===") => 1)
(check (setext-heading-level "---") => 2)
(check (setext-heading-level "  ===") => 1)

(let ((heading (parse-setext-heading '("Hello" "==="))))
  (check (cmark-node-type heading) => 'heading)
  (check (cdr (assoc 'level (cmark-node-data heading))) => 1)
) ;let

(check-report)
