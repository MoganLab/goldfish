;; pi-summarization-failure
;; 根据模型响应生成压缩摘要失败的错误描述。
;;
;; 语法
;; ----
;; (pi-summarization-failure response label)
;;
;; 参数
;; ----
;; response : (or message alist)
;; 模型响应对象。
;;
;; label : string
;; 失败场景标签。
;;
;; 返回值
;; ----
;; string
;; 失败原因描述文本。

(import (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi code core compaction summary)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

;; pi-summarization-failure: error / length 均视为失败
(let ((err-msg (pi-assistant-message (list (pi-text-block ""))
                 :stop-reason   "error"
                 :error-message "boom"
               ) ;pi-assistant-message
      ) ;err-msg
     ) ;
  (check-true (string-contains? (pi-summarization-failure err-msg "Summarization") "boom")
  ) ;check-true
) ;let

(let ((length-msg (pi-assistant-message (list (pi-text-block "partial")) :stop-reason "length")
      ) ;length-msg
     ) ;
  (check-true (string-contains? (pi-summarization-failure length-msg "Summarization")
                "token cap"
              ) ;string-contains?
  ) ;check-true
) ;let

(check
  (pi-summarization-failure (pi-assistant-message (list (pi-text-block "ok")))
    "Summarization"
  ) ;pi-summarization-failure
  =>
  #f
) ;check

(check-report)
