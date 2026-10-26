(import (liii check)
        (liii string)
        (liii pi ai message) (liii pi code core compaction serialize)
        (liii pi code core compaction))

(check-set-mode! 'report-failed)

;; pi-truncate-for-summary
(let ((long (make-string 30 #\x)))
  (check (pi-truncate-for-summary "short" 10) => "short")
  (check (pi-truncate-for-summary long 10)
    => (string-append (make-string 10 #\x)
         "\n\n[... 20 more characters truncated]"
       ) ;string-append
  ) ;check
) ;let

;; pi-serialize-conversation
(let* ((msgs (list (pi-user-message "hello")
                   (pi-assistant-message (list (pi-text-block "world")
                                               (pi-tool-call-block "id-1"
                                                 "read"
                                                 '(("path" . "/tmp/a"))
                                                 #f
                                               ) ;pi-tool-call-block
                                         ) ;list
                   ) ;pi-assistant-message
                   (pi-tool-result-message "id-1" "read" "file body" #f)
                 ) ;list
       ) ;msgs
       (text (pi-serialize-conversation msgs))
      ) ;
  (check-true (string-contains? text "[User]: hello"))
  (check-true (string-contains? text "[Assistant]: world"))
  (check-true (string-contains? text "[Assistant tool calls]: read(path=\"/tmp/a\")"))
  (check-true (string-contains? text "[Tool result]: file body"))
) ;let*

;; thinking 不进入序列化文本
(let ((text (pi-serialize-conversation
              (list (pi-assistant-message (list (pi-thinking-block "secret")
                                                (pi-text-block "visible")
                                              ) ;list
                      ) ;pi-assistant-message
              ) ;list
            ) ;pi-serialize-conversation
      ) ;binding
     ) ;bindings
  (check-false (string-contains? text "secret"))
  (check-true (string-contains? text "visible"))
) ;let

(check-report)
