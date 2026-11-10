;; pi-build-summarization-prompt
;; 构建会话压缩的摘要生成提示词。
;;
;; 语法
;; ----
;; (pi-build-summarization-prompt conversation-text previous-summary)
;;
;; 参数
;; ----
;; conversation-text : string
;; 待压缩的会话文本。
;;
;; previous-summary : (or string #f)
;; 上一次的压缩摘要。
;;
;; 返回值
;; ----
;; string
;; 组装好的摘要生成提示词。

(import (liii check) (liii string) (liii pi code core compaction prompt))

(check-set-mode! 'report-failed)

(let ((p (pi-build-summarization-prompt "[User]: hi" #f)))
  (check-true (string-contains? p "<conversation>\n[User]: hi\n</conversation>"))
  (check-false (string-contains? p "<previous-summary>"))
  (check-true (string-contains? p "## Goal"))
) ;let

(let ((p (pi-build-summarization-prompt "new" "old summary")))
  (check-true (string-contains? p "<previous-summary>\nold summary\n</previous-summary>")
  ) ;check-true
) ;let

(check-report)
