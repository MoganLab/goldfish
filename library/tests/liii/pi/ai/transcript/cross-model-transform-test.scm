;; cross-model-transform
;; 验证转录规范化层对跨模型思维链过滤与思考签名剥离。
;;
;; 语法
;; ----
;; (pi-normalize-transcript messages config)
;;
;; 参数
;; ----
;; messages : list?
;; 内部消息列表（支持 alist 与 record），包含不同服务商/模型的历史助手消息。
;; config : list?
;; 当前请求的目标配置，包含 provider, model-id, provider-api 等字段。
;;
;; 返回值
;; ----
;; list?
;; 规范化后的消息列表：异构思维链被丢弃，工具调用块上的 thoughtSignature 被剥离。

(import (liii check)
  (liii list)
  (liii pi ai message)
  (liii pi ai transcript)
) ;import

(check-set-mode! 'report-failed)

(define (msg-role m) (cdr (assoc "role" m)))
(define (msg-content m) (cdr (assoc "content" m)))
(define (blk-type b) (cdr (assoc "type" b)))
(define (blk-id b) (cdr (assoc "id" b)))
(define (blk-name b) (cdr (assoc "name" b)))

(define (mk-user text)
  (list (cons "role" "user") (cons "content" text))
) ;define

(define (mk-assistant blocks stop prov model api)
  (let ((base (list (cons "role" "assistant")
                (cons "content" blocks)
                (cons "stopReason" stop)
              ) ;list
        ) ;base
       ) ;
    (append base
      (if prov (list (cons "provider" prov)) '())
      (if model (list (cons "model" model)) '())
      (if api (list (cons "api" api)) '())
    ) ;append
  ) ;let
) ;define

(define (mk-thinking text sig)
  (let ((base (list (cons "type" "thinking") (cons "thinking" text))))
    (if sig (append base (list (cons "thinkingSignature" sig))) base)
  ) ;let
) ;define

(define (mk-text text sig)
  (let ((base (list (cons "type" "text") (cons "text" text))))
    (if sig (append base (list (cons "textSignature" sig))) base)
  ) ;let
) ;define

(define (mk-tool-call id name args sig)
  (let ((base (list (cons "type" "toolCall")
                (cons "id" id)
                (cons "name" name)
                (cons "arguments" args)
              ) ;list
        ) ;base
       ) ;
    (if sig (append base (list (cons "thoughtSignature" sig))) base)
  ) ;let
) ;define

(define (mk-tool-result id name text)
  (list (cons "role" "toolResult")
    (cons "toolCallId" id)
    (cons "toolName" name)
    (cons "content" (list (mk-text text #f)))
    (cons "isError" #f)
  ) ;list
) ;define

(define gemini-cfg
  '((provider . "google")
    (model-id . "gemini-2.5-flash")
    (provider-api . "google-generative-ai")
   )
) ;define

(define gemini-3-cfg
  '((provider . "google")
    (model-id . "gemini-3-flash-preview")
    (provider-api . "google-generative-ai")
   )
) ;define

;; ============ 1. 同模型保留 thinking 与 thoughtSignature ============

(let* ((msgs (list (mk-user "计算 1+1")
               (mk-assistant
                 (list (mk-thinking "正在思考 1+1..." "SIG_GEMINI_1234")
                   (mk-text "1+1 等于 2" "SIG_TEXT_1234")
                   (mk-tool-call "call_calc_1" "calc" '(("expr" . "1+1")) "SIG_TC_1234")
                 ) ;list
                 "toolUse"
                 "google"
                 "gemini-2.5-flash"
                 "google-generative-ai"
               ) ;mk-assistant
               (mk-tool-result "call_calc_1" "calc" "2")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
       (asst (list-ref out 1))
       (content (msg-content asst))
      ) ;
  (check (length content) => 3)
  ;; thinking 块完整保留
  (let ((th-blk (list-ref content 0)))
    (check (blk-type th-blk) => "thinking")
    (check (cdr (assoc "thinking" th-blk)) => "正在思考 1+1...")
    (check (cdr (assoc "thinkingSignature" th-blk)) => "SIG_GEMINI_1234")
  ) ;let
  ;; text 块签名保留
  (let ((tx-blk (list-ref content 1)))
    (check (blk-type tx-blk) => "text")
    (check (cdr (assoc "text" tx-blk)) => "1+1 等于 2")
    (check (cdr (assoc "textSignature" tx-blk)) => "SIG_TEXT_1234")
  ) ;let
  ;; toolCall 块签名保留
  (let ((tc-blk (list-ref content 2)))
    (check (blk-type tc-blk) => "toolCall")
    (check (cdr (assoc "thoughtSignature" tc-blk)) => "SIG_TC_1234")
  ) ;let
) ;let*

;; ============ 2. 异构模型丢弃 thinking 并剥离 thoughtSignature ============

;; 历史消息来自 MiniMax，当前请求发往 Gemini
(let* ((msgs (list (mk-user "查找文件")
               (mk-assistant
                 (list (mk-thinking "MiniMax 内部推理：先用 bash ls 查看" "SIG_MINIMAX_THINKING")
                   (mk-text "我将为您执行命令" "SIG_MINIMAX_TEXT")
                   (mk-tool-call "call_ls_1" "bash" '(("cmd" . "ls")) "SIG_MINIMAX_TC")
                 ) ;list
                 "toolUse"
                 "minimax-cn"
                 "MiniMax-M2.7"
                 "anthropic-messages"
               ) ;mk-assistant
               (mk-tool-result "call_ls_1" "bash" "a.txt\nb.txt")
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
       (asst (list-ref out 1))
       (content (msg-content asst))
      ) ;
  ;; 异构思维链被丢弃，仅剩 text 和 toolCall 两个块
  (check (length content) => 2)
  ;; 第一个块应为 text，且私有签名已剥离
  (let ((tx-blk (list-ref content 0)))
    (check (blk-type tx-blk) => "text")
    (check (cdr (assoc "text" tx-blk)) => "我将为您执行命令")
    (check (assoc "textSignature" tx-blk) => #f)
  ) ;let
  ;; 第二个块应为 toolCall，thoughtSignature 已安全移除
  (let ((tc-blk (list-ref content 1)))
    (check (blk-type tc-blk) => "toolCall")
    (check (blk-id tc-blk) => "call_ls_1")
    (check (blk-name tc-blk) => "bash")
    (check (assoc "thoughtSignature" tc-blk) => #f)
  ) ;let
  ;; toolResult 不受影响
  (check (msg-role (list-ref out 2)) => "toolResult")
  (check (cdr (assoc "toolCallId" (list-ref out 2))) => "call_ls_1")
) ;let*

;; ============ 3. 缺失模型元数据兜底按跨模型安全剥离 ============

(let* ((msgs (list (mk-assistant
                     (list (mk-thinking "无元数据思考" "SIG_UNKNOWN")
                       (mk-tool-call "call_1" "read" '() "SIG_UNKNOWN_TC")
                     ) ;list
                     "toolUse"
                     #f
                     #f
                     #f
                   ) ;mk-assistant
                   (mk-tool-result "call_1" "read" "file content")
                 ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
       (content (msg-content (list-ref out 0)))
      ) ;
  ;; thinking 块被丢弃
  (check (length content) => 1)
  (let ((tc (car content)))
    (check (blk-type tc) => "toolCall")
    (check (assoc "thoughtSignature" tc) => #f)
  ) ;let
) ;let*

;; ============ 4. 多轮混合模型接力对话 ============

(let* ((msgs (list (mk-user "第1轮问答")
               ;; 第1轮：MiniMax
               (mk-assistant
                 (list (mk-thinking "MiniMax 第1轮思考" "MM_SIG")
                   (mk-text "MiniMax 第1轮回复" #f)
                 ) ;list
                 "stop"
                 "minimax-cn"
                 "MiniMax-M2.7"
                 "anthropic-messages"
               ) ;mk-assistant
               (mk-user "第2轮问答")
               ;; 第2轮：Gemini
               (mk-assistant
                 (list (mk-thinking "Gemini 第2轮思考" "GEMINI_SIG")
                   (mk-text "Gemini 第2轮回复" #f)
                 ) ;list
                 "stop"
                 "google"
                 "gemini-2.5-flash"
                 "google-generative-ai"
               ) ;mk-assistant
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
       (c1 (msg-content (list-ref out 1)))
       (c2 (msg-content (list-ref out 3)))
      ) ;
  ;; MiniMax 的 thinking 块丢弃，只剩 text
  (check (length c1) => 1)
  (check (blk-type (car c1)) => "text")
  (check (cdr (assoc "text" (car c1))) => "MiniMax 第1轮回复")
  ;; Gemini 的 thinking 块保留
  (check (length c2) => 2)
  (check (blk-type (car c2)) => "thinking")
  (check (cdr (assoc "thinking" (car c2))) => "Gemini 第2轮思考")
) ;let*

;; ============ 5. record 对象输入形态支持 ============

(let* ((rec-assistant
         (pi-assistant-message
           (list (pi-thinking-block "MiniMax record 思维链" "MM_REC_SIG")
             (pi-tool-call-block "tc_rec_1" "edit" '(("path" . "a.scm")) "MM_TC_SIG")
           ) ;list
           :stop-reason "toolUse"
           :provider    "minimax-cn"
           :model       "MiniMax-M2.7"
           :api         "anthropic-messages"
         ) ;pi-assistant-message
       ) ;rec-assistant
       (msgs (list (pi-user-message "编辑文件")
               rec-assistant
               (pi-tool-result-message "tc_rec_1" "edit" (list (pi-text-block "ok")))
             ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-cfg))
       (asst (list-ref out 1))
       (content (msg-content asst))
      ) ;
  ;; 跨模型时 thinking 块丢弃，只剩 toolCall
  (check (length content) => 1)
  (let ((tc (car content)))
    (check (blk-type tc) => "toolCall")
    (check (blk-id tc) => "tc_rec_1")
    (check (assoc "thoughtSignature" tc) => #f)
  ) ;let
  (check (msg-role (list-ref out 2)) => "toolResult")
) ;let*

;; ============ 6. 跨模型同时触发 Gemini 3+ toolCall id 归一化 ============

(let* ((msgs (list (mk-assistant
                     (list (mk-thinking "MiniMax 思考" "SIG")
                       (mk-tool-call "call|strange:id" "read" '() "MM_SIGNATURE")
                     ) ;list
                     "toolUse"
                     "minimax-cn"
                     "MiniMax-M2.7"
                     "anthropic-messages"
                   ) ;mk-assistant
                   (mk-tool-result "call|strange:id" "read" "content")
                 ) ;list
       ) ;msgs
       (out (pi-normalize-transcript msgs gemini-3-cfg))
       (tc (car (msg-content (list-ref out 0))))
       (tr (list-ref out 1))
      ) ;
  ;; 1. thinking 丢弃，toolCall 保留
  (check (blk-type tc) => "toolCall")
  ;; 2. thoughtSignature 被剥离
  (check (assoc "thoughtSignature" tc) => #f)
  ;; 3. toolCall id 在 Gemini 3+ 下归一化
  (check (blk-id tc) => "call_strange_id")
  ;; 4. toolResult 中的 toolCallId 联动更新
  (check (cdr (assoc "toolCallId" tr)) => "call_strange_id")
) ;let*

(check-report)
