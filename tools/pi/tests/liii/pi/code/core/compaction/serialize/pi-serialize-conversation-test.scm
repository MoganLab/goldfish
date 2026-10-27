(import (liii check)
  (liii string)
  (liii pi ai message)
  (liii pi code core compaction serialize)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

(let* ((msgs
         (list (pi-user-message "hello")
           (pi-assistant-message
             (list (pi-text-block "world")
               (pi-tool-call-block "id-1" "read" '(("path" . "/tmp/a")) #f)
             ) ;list
           ) ;pi-assistant-message
           (pi-tool-result-message "id-1" "read" "file body" #f)
         ) ;list
       ) ;msgs
       (text (pi-serialize-conversation msgs))
      ) ;
  (check-true (string-contains? text "[User]: hello"))
  (check-true (string-contains? text "[Assistant]: world"))
  (check-true (string-contains? text "[Assistant tool calls]: read(path=\"/tmp/a\")")
  ) ;check-true
  (check-true (string-contains? text "[Tool result]: file body"))
) ;let*

;; thinking 不进入序列化文本
(let ((text
        (pi-serialize-conversation
          (list (pi-assistant-message (list (pi-thinking-block "secret") (pi-text-block "visible"))
                ) ;pi-assistant-message
          ) ;list
        ) ;pi-serialize-conversation
      ) ;text
     ) ;
  (check-false (string-contains? text "secret"))
  (check-true (string-contains? text "visible"))
) ;let

(check-report)
