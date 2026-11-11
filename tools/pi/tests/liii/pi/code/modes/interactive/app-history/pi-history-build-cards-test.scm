;; pi-history-build-cards
;; 把会话条目转换为可见的 TUI 卡片（历史重建的纯函数入口）。
;;
;; 语法
;; ----
;; (pi-history-build-cards entries hide-thinking? tools-expanded?)
;;
;; 参数
;; ----
;; entries : list
;; 会话条目列表（pi-build-session-context-entries 的结果）。
;; hide-thinking? : boolean
;; 思考卡片是否折叠（Ctrl+T 的全局隐藏偏好）。
;; tools-expanded? : boolean
;; 工具卡片是否展开（Ctrl+O 的全局展开偏好）。
;;
;; 返回值
;; ----
;; (values cards index)
;; cards : list
;; 按条目顺序排列的展示卡片。
;; index : alist
;; toolCallId → 工具卡片。

(import (scheme base)
  (liii check)
  (liii string)
  (liii tui)
  (liii pi code modes interactive app-history)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 卡片的纯文本内容（去掉 ANSI 颜色码）
(define (card-text card)
  (string-join (map strip-ansi (component-render card 100)) "\n")
) ;define

;; 用户消息条目
(define (make-user-entry id parent-id text)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" "user") (cons "content" text)))
  ) ;list
) ;define

;; 助手消息条目（内容为块数组）
(define (make-assistant-entry id parent-id blocks)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message" (list (cons "role" "assistant") (cons "content" blocks)))
  ) ;list
) ;define

;; 助手文本块
(define (text-block text)
  (list (cons "type" "text") (cons "text" text))
) ;list

;; 助手思考块
(define (thinking-block text)
  (list (cons "type" "thinking") (cons "thinking" text))
) ;list

;; 助手工具调用块
(define (tool-call-block id name args)
  (list (cons "type" "toolCall")
    (cons "id" id)
    (cons "name" name)
    (cons "arguments" args)
  ) ;list
) ;define

;; 工具结果条目
(define (make-tool-result-entry id parent-id call-id name text err?)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message"
      (list (cons "role" "toolResult")
        (cons "toolCallId" call-id)
        (cons "toolName" name)
        (cons "content" (list (text-block text)))
        (cons "isError" err?)
      ) ;list
    ) ;list
  ) ;list
) ;define

;; bash 执行条目
(define (make-bash-entry id parent-id command output code excluded?)
  (list (cons "type" "message")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "message"
      (append
        (list (cons "role" "bashExecution")
          (cons "command" command)
          (cons "output" output)
          (cons "exitCode" code)
        ) ;list
        (if excluded? (list (cons "excludeFromContext" #t)) '())
      ) ;append
    ) ;list
  ) ;list
) ;define

;; 压缩条目
(define (make-compaction-entry id parent-id summary)
  (list (cons "type" "compaction")
    (cons "id" id)
    (cons "parentId" parent-id)
    (cons "summary" summary)
    (cons "firstKeptEntryId" parent-id)
  ) ;list
) ;define

;; 1. 空历史：没有卡片，也没有工具索引
(let*-values (((cards index) (pi-history-build-cards '() #f #f))) ;
  (check cards => '())
  (check index => '())
) ;let*-values

;; 2. 用户与助手消息按原顺序生成卡片
(let*-values (((cards index)
               (pi-history-build-cards
                 (list (make-user-entry "u1" 'null "第一条提问")
                   (make-assistant-entry "a1" "u1"
                     (list (text-block "第一条回答")))
                 )
                 #f
                 #f
               ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 2)
  (check index => '())
  (check-true (string-contains? (card-text (car cards)) "第一条提问"))
  (check-true (string-contains? (card-text (cadr cards)) "第一条回答"))
) ;let*-values

;; 3. 多行用户文本完整保留
(let*-values (((cards _index) (pi-history-build-cards
                   (list (make-user-entry "u1" 'null "第一行\n第二行"))
                   #f
                   #f
                 ) ;pi-history-build-cards
              ) ;
             ) ;
  (check-true (string-contains? (card-text (car cards)) "第一行"))
  (check-true (string-contains? (card-text (car cards)) "第二行"))
) ;let*-values

;; 4. 助手内容块按顺序展开：text → thinking → toolCall
(let*-values (((cards index)
               (pi-history-build-cards
                 (list
                   (make-assistant-entry "a1" 'null
                     (list
                       (text-block "回答正文")
                       (thinking-block "推理过程")
                       (tool-call-block "call-1" "mytool" (list (cons "path" "/tmp/x")))
                     ) ;list
                   ) ;make-assistant-entry
                 )
                 #f
                 #f
               ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 3)
  (check-true (string-contains? (card-text (car cards)) "回答正文"))
  (check-true (string-contains? (card-text (cadr cards)) "推理过程"))
  (check-true (string-contains? (card-text (caddr cards)) "mytool"))
  ;; 工具卡片索引按 toolCallId 建立
  (check (length index) => 1)
  (check (cdr (assoc "call-1" index)) => (caddr cards))
) ;let*-values

;; 5. 思考卡片服从 hide-thinking 偏好
(let*-values (((hidden-cards _hc-index) (pi-history-build-cards
                             (list (make-assistant-entry "a1" 'null
                               (list (thinking-block "秘密推理"))
                             ) ;make-assistant-entry
                           ) ;list
                           #t
                           #f
                         ) ;pi-history-build-cards
              ) ;
              ((shown-cards _sc-index) (pi-history-build-cards
                               (list (make-assistant-entry "a1" 'null
                                 (list (thinking-block "秘密推理"))
                               ) ;make-assistant-entry
                             ) ;list
                             #f
                             #f
                           ) ;pi-history-build-cards
              ) ;
             ) ;
  (check-true (string-contains? (card-text (car hidden-cards)) "Thinking..."))
  (check-false (string-contains? (card-text (car hidden-cards)) "秘密推理"))
  (check-true (string-contains? (card-text (car shown-cards)) "秘密推理"))
) ;let*-values

;; 6. toolResult 回填同一张工具卡片：不另起独立结果卡片
(let*-values (((cards index)
               (pi-history-build-cards
                 (list
                   (make-assistant-entry "a1" 'null
                     (list (tool-call-block "call-1" "mytool" '()))
                   ) ;make-assistant-entry
                   (make-tool-result-entry "r1" "a1" "call-1" "mytool" "工具输出内容" #f)
                 )
                 #f
                 #f
               ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 1)
  (check-true (string-contains? (card-text (car cards)) "工具输出内容"))
  (check (length index) => 1)
) ;let*-values

;; 7. 错误结果同样回填同一张卡片
(let*-values (((cards _index) (pi-history-build-cards
                   (list
                     (make-assistant-entry "a1" 'null
                       (list (tool-call-block "call-1" "mytool" '()))
                     ) ;make-assistant-entry
                     (make-tool-result-entry "r1" "a1" "call-1" "mytool" "工具失败原因" #t)
                   )
                   #f
                   #f
                 ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 1)
  (check-true (string-contains? (card-text (car cards)) "工具失败原因"))
) ;let*-values

;; 8. 没有结果的历史工具调用不留在 pending：给出明确终态
(let*-values (((cards _index) (pi-history-build-cards
                   (list (make-assistant-entry "a1" 'null
                     (list (tool-call-block "call-1" "mytool" '()))
                   ) ;make-assistant-entry
                   )
                   #f
                   #f
                 ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 1)
  (check-true (string-contains? (card-text (car cards)) "Tool result not found"))
) ;let*-values

;; 9. 压缩条目形成摘要卡片，与真实用户输入区分
(let*-values (((cards _index) (pi-history-build-cards
                   (list (make-compaction-entry "c1" 'null "早期对话摘要")
                     (make-user-entry "u1" "c1" "压缩之后的新提问")
                   )
                   #f
                   #f
                 ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 2)
  (check-true (string-contains? (card-text (car cards)) "[compacted]"))
  (check-true (string-contains? (card-text (car cards)) "早期对话摘要"))
  (check-true (string-contains? (card-text (cadr cards)) "压缩之后的新提问"))
) ;let*-values

;; 10. bash 执行条目复用 bash 卡片：命令、输出与退出码来自条目本身
(let*-values (((cards _index) (pi-history-build-cards
                   (list (make-bash-entry "b1" 'null "echo hi" "hi\n" 0 #f))
                   #f
                   #f
                 ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 1)
  (check-true (string-contains? (card-text (car cards)) "echo hi"))
  (check-true (string-contains? (card-text (car cards)) "hi"))
) ;let*-values

;; 11. excludeFromContext 的 bash 执行仍可见（只影响模型上下文）
(let*-values (((cards _index) (pi-history-build-cards
                   (list (make-bash-entry "b1" 'null "echo secret" "secret\n" 0 #t))
                   #f
                   #f
                 ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 1)
  (check-true (string-contains? (card-text (car cards)) "echo secret"))
) ;let*-values

;; 12. 未知条目类型不产生卡片
(let*-values (((cards _index) (pi-history-build-cards
                   (list (list (cons "type" "mystery") (cons "id" "x"))
                     (make-user-entry "u1" 'null "正常消息")
                   )
                   #f
                   #f
                 ) ;pi-history-build-cards
              ) ;
             ) ;
  (check (length cards) => 1)
  (check-true (string-contains? (card-text (car cards)) "正常消息"))
) ;let*-values

(check-report)
