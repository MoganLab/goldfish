(import (liii check)
  (liii pi ai message)
  (liii pi code core compaction tokens)
  (liii pi code core session-manager)
  (liii pi code core compaction)
) ;import

(check-set-mode! 'report-failed)

;; pi-context-token-estimate
;; 估算当前上下文的 token 数，优先采用最后一次有效助手响应的 usage。
;;
;; 语法
;; ----
;; (pi-context-token-estimate messages) -> alist
;;
;; 参数
;; ----
;; messages : list
;; 会话消息列表，可包含消息 record 或 alist。
;;
;; 返回值
;; ----
;; alist
;; tokens 为总估算值，usage-tokens 为最后一次有效响应的用量，
;; trailing-tokens 为该响应之后消息的估算值。
;; 没有有效 usage 时，全部消息按约四字符一个 token 估算。
;;
;; 示例
;; ----
;; (pi-context-token-estimate messages)
;; => '((tokens . 102) (usage-tokens . 100) (trailing-tokens . 2))

;; pi-context-token-estimate
;; 无 usage 时全部按 chars/4 估算
(let ((est (pi-context-token-estimate (list (pi-user-message "abcdefgh")))))
  (check (cdr (assoc 'tokens est)) => 2)
  (check (cdr (assoc 'usage-tokens est)) => 0)
  (check (cdr (assoc 'trailing-tokens est)) => 2)
) ;let

;; 末条有效 assistant usage + 其后消息估算
(let* ((msgs
         (list (pi-user-message "abcdefgh")
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
(let* ((msgs
         (list (pi-assistant-message (list (pi-text-block "abcd"))
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
