;; parse-link-destination
;; 规范化并解码链接目标与可选标题字符串。
;;
;; 语法
;; ----
;; (parse-link-destination dest)
;;
;; 参数
;; ----
;; dest : string
;; 原始目标与标题文本。
;;
;; 返回值
;; ----
;; pair
;; 返回 (url . title) 点对。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link))

(check-set-mode! 'report-failed)

(check (parse-link-destination "https://example.com \"T\"")
  =>
  '("https://example.com" . "T")
) ;check

(check-report)
