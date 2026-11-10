;; pi-format-compaction-summary
;; 将压缩结果格式化为一行摘要（压缩条数、保留条数与压缩前 Token 数）。
;;
;; 语法
;; ----
;; (pi-format-compaction-summary result . opt-label)
;;
;; 参数
;; ----
;; result : alist
;; 压缩结果 alist，含 summarized-count、kept-count、tokens-before 三项。
;; opt-label : string
;; 可选摘要前缀标签；缺省为 "Compacted"。
;;
;; 返回值
;; ----
;; string
;; 形如 "Compacted session: summarized 3 messages, kept 5, tokens before 1200" 的摘要串。

(import (scheme base) (liii check) (liii pi code core compaction flow))

(check-set-mode! 'report-failed)

(define result
  (list (cons 'summarized-count 3)
    (cons 'kept-count 5)
    (cons 'tokens-before 1200)
  ) ;list
) ;define

;; 1. 默认标签
(check (pi-format-compaction-summary result)
  =>
  "Compacted session: summarized 3 messages, kept 5, tokens before 1200"
) ;check

;; 2. 自定义标签（自动压缩场景）
(check (pi-format-compaction-summary result "Auto-compacted")
  =>
  "Auto-compacted session: summarized 3 messages, kept 5, tokens before 1200"
) ;check

;; 3. 标签非字符串时回退默认标签
(check (pi-format-compaction-summary result 42)
  =>
  "Compacted session: summarized 3 messages, kept 5, tokens before 1200"
) ;check

(check-report)
