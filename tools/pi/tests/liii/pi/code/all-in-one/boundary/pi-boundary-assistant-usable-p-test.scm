;; pi-boundary-assistant-usable?
;; 判断 assistant 消息是否可安全用于跨模型交接。
;;
;; 语法
;; ----
;; (pi-boundary-assistant-usable? message)
;;
;; 参数
;; ----
;; message : any
;; 待判断的 assistant 消息记录或 alist。
;;
;; 返回值
;; ----
;; boolean
;; 完整回复为 #t；报错、中止、截断、空内容或悬空调用为 #f。

(import (scheme base) (liii check) (liii pi ai message)
  (liii pi code all-in-one boundary)
) ;import

(check-set-mode! 'report-failed)

(check-true
  (pi-boundary-assistant-usable?
    (pi-assistant-message (list (pi-text-block "done")) "stop")
  ) ;pi-boundary-assistant-usable?
) ;check-true
(check-false
  (pi-boundary-assistant-usable?
    (pi-assistant-message (list (pi-text-block "partial")) "error" "HTTP 529")
  ) ;pi-boundary-assistant-usable?
) ;check-false
(check-false
  (pi-boundary-assistant-usable?
    (pi-assistant-message (list (pi-text-block "partial")) "aborted")
  ) ;pi-boundary-assistant-usable?
) ;check-false
(check-false
  (pi-boundary-assistant-usable?
    (pi-assistant-message (list (pi-text-block "truncated")) "length")
  ) ;pi-boundary-assistant-usable?
) ;check-false
(check-false
  (pi-boundary-assistant-usable?
    (pi-assistant-message '() "aborted")
  ) ;pi-boundary-assistant-usable?
) ;check-false
(check-false
  (pi-boundary-assistant-usable?
    (pi-assistant-message (list (pi-tool-call-block "c1" "read" '())) "toolUse")
  ) ;pi-boundary-assistant-usable?
) ;check-false
(check-false
  (pi-boundary-assistant-usable?
    '((role . "assistant") (content . "partial") (stopReason . "error"))
  ) ;pi-boundary-assistant-usable?
) ;check-false

(check-report)
