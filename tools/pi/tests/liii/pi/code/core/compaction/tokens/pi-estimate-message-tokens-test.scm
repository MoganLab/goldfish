(import (liii check)
  (liii pi ai message)
  (liii pi code core compaction tokens)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

(check (pi-estimate-message-tokens (pi-user-message "hello world!")) => 3)
(check (pi-estimate-message-tokens (pi-user-message "")) => 0)
(check
  (pi-estimate-message-tokens (pi-user-message (list (pi-text-block "abcd"))))
  =>
  1
) ;check

;; assistant 的 thinking 与 toolCall 也计入估算
(check-true
  (>
    (pi-estimate-message-tokens
      (pi-assistant-message
        (list (pi-text-block "abcd")
          (pi-tool-call-block "id-1" "read" '(("path" . "x")) #f)
        ) ;list
      ) ;pi-assistant-message
    ) ;pi-estimate-message-tokens
    1
  ) ;>
) ;check-true

(check-report)
