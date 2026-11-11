;; DeepSeek 聊天 demo：基于 (liii pi ai) 的流式多轮对话
;;
;; 凭据：~/.gf/agent/auth.json（(liii pi ai auth)）→ DEEPSEEK_API_KEY → 终端手动输入
;; 协议：OpenAI Completions 兼容接口（provider-api = "openai-completions"）
;; 请求：(liii pi ai dispatch) 的 pi-request-provider-stream —— 与 tools/pi 实际请求同一条链路；
;;       端点/请求头/请求体由 (liii pi ai api openai-completions) 生成，
;;       流式响应用 (liii sse) 的 make-sse-decoder 解码
;; 多轮：(liii pi ai message) 的消息结构，每次请求携带全量对话历史
;;
;; 运行: bin/gf demo/liii/pi/ai/deepseek_chat.scm

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii path)
  (liii string)
  (liii pi ai auth)
  (liii pi ai dispatch)
  (liii pi ai message)
) ;import

(define base-url
  (or (get-environment-variable "DEEPSEEK_BASE_URL") "https://api.deepseek.com")
) ;define

(define model-id
  (or (get-environment-variable "DEEPSEEK_MODEL") "deepseek-chat")
) ;define

;; 从 ~/.gf/agent/auth.json 读取 DeepSeek 凭据（回退到环境变量）

(define (resolve-api-key)
  (let ((key
          (pi-resolve-provider-key "deepseek"
            (pi-load-auth (path-join (path-home) ".gf" "agent"))
          ) ;pi-resolve-provider-key
        ) ;key
       ) ;
    (if (and (string? key) (not (string-null? key))) key #f)
  ) ;let
) ;define

;; 兼容跨平台的终端标准输入端口

(define stdin-port
  (if (file-exists? "/dev/stdin")
    (open-input-file "/dev/stdin")
    (current-input-port)
  ) ;if
) ;define

;; 读取一行；EOF 时返回 #f，空行返回 ""

(define (prompt-line prompt)
  (display prompt)
  (flush-output-port)
  (let ((line (read-line stdin-port)))
    (if (eof-object? line) #f (string-trim line))
  ) ;let
) ;define

;; 手动输入 API Key

(define (prompt-api-key)
  (let ((key (prompt-line "未找到 DeepSeek API Key，请输入: ")))
    (if (or (not key) (string-null? key)) #f key)
  ) ;let
) ;define

;; 流式事件：把 text_delta 增量实时打印到终端

(define (on-stream-event ev)
  (when (equal? (cdr (assoc 'type ev)) "text_delta")
    (let ((delta (cdr (assoc 'delta ev))))
      (when (string? delta)
        (display delta)
        (flush-output-port)
      ) ;when
    ) ;let
  ) ;when
) ;define

;; 发起一轮请求：实时输出增量文本，返回助手消息（失败返回 #f）

(define (ask api-key messages)
  (let* ((cfg (pi-make-request-config "deepseek" model-id api-key
                :base-url base-url
                :provider-api "openai-completions"
              ) ;pi-make-request-config
         ) ;cfg
         (resp (pi-request-provider-stream "deepseek" cfg messages #f '() on-stream-event 300)
         ) ;resp
        ) ;
    (newline)
    (if (equal? (pi-assistant-message-stop-reason resp) "error")
      (begin
        (display "请求失败：")
        (display (or (pi-assistant-message-error-message resp) "未知错误"))
        (newline)
        #f
      ) ;begin
      resp
    ) ;if
  ) ;let*
) ;define

;; 交互循环：history 保存全量对话（用户消息与助手消息交替）

(define (chat-loop api-key)
  (let loop
    ((history '()))
    (let ((question (prompt-line "\nQ> ")))
      (cond ((or (not question) (string=? question "quit") (string=? question "exit"))
             (display "再见！\n")
            ) ;
            ((string-null? question) (loop history))
            (else
              (let* ((user-msg (pi-user-message question))
                     (resp (ask api-key (append history (list user-msg))))
                    ) ;
                (if resp (loop (append history (list user-msg resp))) (loop history))
              ) ;let*
            ) ;else
      ) ;cond
    ) ;let
  ) ;let
) ;define

(define (main)
  (let ((key (or (resolve-api-key) (prompt-api-key))))
    (if (not key)
      (begin
        (display "未获取到 DeepSeek API Key，退出。\n")
        (exit 1)
      ) ;begin
      (begin
        (display (string-append "DeepSeek 聊天已就绪（model: " model-id "）\n"))
        (display "输入问题开始对话，输入 'quit' 或 'exit' 退出。\n")
        (chat-loop key)
      ) ;begin
    ) ;if
  ) ;let
) ;define

(main)
