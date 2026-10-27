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
;; (pi-context-token-estimate messages) => pi-context-token-estimate
;;
;; 参数
;; ----
;; messages : list
;; 会话消息列表，可包含消息 record 或 alist。
;;
;; 返回值
;; ----
;; pi-context-token-estimate
;; 可通过访问器读取 tokens、usage-tokens 和 trailing-tokens 的 record。
;; 没有有效 usage 时，全部消息按约四字符一个 token 估算。
;;
;; 示例
;; ----
;; (pi-context-token-estimate-tokens (pi-context-token-estimate messages)) => 102

;; pi-context-token-estimate
;; 无 usage 时全部按 chars/4 估算
(let ((est (pi-context-token-estimate (list (pi-user-message "abcdefgh")))))
  (check (pi-context-token-estimate-tokens est) => 2)
  (check (pi-context-token-estimate-usage-tokens est) => 0)
  (check (pi-context-token-estimate-trailing-tokens est) => 2)
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
  (check (pi-context-token-estimate-usage-tokens est) => 100)
  (check (pi-context-token-estimate-trailing-tokens est) => 2)
  (check (pi-context-token-estimate-tokens est) => 102)
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
  (check (pi-context-token-estimate-usage-tokens est) => 15)
  (check (pi-context-token-estimate-tokens est) => 15)
) ;let*

(check-report)
