;; pi-estimate-content-chars
;; 估算消息内容的字符数。
;;
;; 语法
;; ----
;; (pi-estimate-content-chars content)
;;
;; 参数
;; ----
;; content : any
;; 消息内容，支持字符串、文本块列表与 JSON 解出的向量。
;;
;; 返回值
;; ----
;; integer
;; 估算出的字符数。

(import (liii check)
  (liii pi ai message)
  (liii pi code core compaction tokens)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

(check (pi-estimate-content-chars "abcdefgh") => 8)
(check (pi-estimate-content-chars (list (pi-text-block "ab") (pi-text-block "cde")))
  =>
  5
) ;check
(check (pi-estimate-content-chars "") => 0)

(check-report)
