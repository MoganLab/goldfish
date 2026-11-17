(import (liii check) (liii cmark block parser) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-setext-heading
;; 解析 Setext 风格标题。
;;
;; 语法
;; ----
;; (parse-setext-heading lines)
;;
;; 描述
;; ----
;; setext-underline? 与 setext-heading-level 用于识别并确定标题级别。

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
