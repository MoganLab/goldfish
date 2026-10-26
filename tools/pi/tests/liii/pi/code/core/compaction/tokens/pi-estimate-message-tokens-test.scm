(import (liii check)
        (liii pi ai message) (liii pi code core compaction tokens)
        (liii pi code core compaction))

(check-set-mode! 'report-failed)

;; pi-estimate-message-tokens
;; chars/4 向上取整（对齐 pi estimateTokens）
(check (pi-estimate-message-tokens (pi-user-message "hello world!")) => 3)
(check (pi-estimate-message-tokens (pi-user-message "")) => 0)
(check (pi-estimate-message-tokens
         (pi-user-message (list (pi-text-block "abcd"))))
       => 1)

;; pi-estimate-content-chars
(check (pi-estimate-content-chars "abcdefgh") => 8)
(check (pi-estimate-content-chars (list (pi-text-block "ab")
                                        (pi-text-block "cde")))
       => 5)
(check (pi-estimate-content-chars "") => 0)

;; assistant 的 thinking 与 toolCall 也计入估算
(check-true (> (pi-estimate-message-tokens
                 (pi-assistant-message (list (pi-text-block "abcd")
                                             (pi-tool-call-block "id-1"
                                               "read"
                                               '(("path" . "x"))
                                               #f
                                             ) ;pi-tool-call-block
                                       ) ;list
                 ) ;pi-assistant-message
               ) 1
             ) ;>
) ;check-true

(check-report)
