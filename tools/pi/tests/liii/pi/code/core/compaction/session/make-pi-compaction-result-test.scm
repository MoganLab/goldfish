;; make-pi-compaction-result
;; 构造会话压缩结果记录。
;;
;; 语法
;; ----
;; (make-pi-compaction-result id summary first-kept-entry-id tokens-before
;;   summarized-count kept-count usage)
;;
;; 参数
;; ----
;; id : string
;; 压缩条目 ID。
;; summary : string
;; 压缩摘要文本。
;; first-kept-entry-id : string
;; 首个保留条目的 ID。
;; tokens-before : integer
;; 压缩前 token 数量。
;; summarized-count : integer
;; 被摘要的消息条数。
;; kept-count : integer
;; 保留的消息条数。
;; usage : list
;; 模型用量信息。
;;
;; 返回值
;; ----
;; pi-compaction-result?
;; 压缩结果记录。

(import (scheme base) (liii check) (liii pi code core compaction session))

(check-set-mode! 'report-failed)

(let ((res (make-pi-compaction-result "c-1" "summary text" "e-10" 1200 3 5 '())))
  (check-true (pi-compaction-result? res))
  (check-false (pi-compaction-result? '()))
  (check (pi-compaction-result-id res) => "c-1")
  (check (pi-compaction-result-summary res) => "summary text")
  (check (pi-compaction-result-first-kept-entry-id res) => "e-10")
  (check (pi-compaction-result-tokens-before res) => 1200)
  (check (pi-compaction-result-summarized-count res) => 3)
  (check (pi-compaction-result-kept-count res) => 5)
  (check (pi-compaction-result-usage res) => '())
) ;let

(check-report)
