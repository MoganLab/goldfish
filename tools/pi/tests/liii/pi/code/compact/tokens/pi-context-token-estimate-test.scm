(import (liii check)
        (liii pi ai message) (liii pi code compact tokens) (liii pi code session)
        (liii pi code compact))

(check-set-mode! 'report-failed)

;; pi-context-token-estimate
;; 无 usage 时全部按 chars/4 估算
(let ((est (pi-context-token-estimate (list (pi-user-message "abcdefgh")))))
  (check (cdr (assoc 'tokens est)) => 2)
  (check (cdr (assoc 'usage-tokens est)) => 0)
  (check (cdr (assoc 'trailing-tokens est)) => 2)
) ;let

;; 末条有效 assistant usage + 其后消息估算
(let* ((msgs (list (pi-user-message "abcdefgh")
                   (pi-assistant-message (list (pi-text-block "abcd"))
                     :usage '((totalTokens . 100))
                   ) ;pi-assistant-message
                   (pi-user-message "12345678")
                 ) ;list
       ) ;msgs
       (est (pi-context-token-estimate msgs))
      ) ;
  (check (cdr (assoc 'usage-tokens est)) => 100)
  (check (cdr (assoc 'trailing-tokens est)) => 2)
  (check (cdr (assoc 'tokens est)) => 102)
) ;let*

;; usage 无 totalTokens 时用 input+output+cacheRead+cacheWrite
(let* ((msgs (list (pi-assistant-message (list (pi-text-block "abcd"))
                     :usage '((input . 10) (output . 5))
                   ) ;pi-assistant-message
                 ) ;list
       ) ;msgs
       (est (pi-context-token-estimate msgs))
      ) ;
  (check (cdr (assoc 'usage-tokens est)) => 15)
  (check (cdr (assoc 'tokens est)) => 15)
) ;let*

(check-report)
