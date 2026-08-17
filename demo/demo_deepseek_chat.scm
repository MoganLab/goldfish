;; DeepSeek 聊天 demo：使用 (liii http) 流式访问 DeepSeek API
;; 配置从 ~/.deepseek.json 读取，格式：
;;   {"api_key": "sk-..."
;;    "base_url": "https://api.deepseek.com"
;;    "model": "deepseek-v4-flash"}
;; 若配置文件缺失，则提示用户手动输入 API Key。
;; 多轮对话：每次请求携带全量对话历史。
;; 流式返回时，每当积累的内容达到 100 个字符（UTF-8）就 flush 一次到终端。
;; 运行: bin/gf demo/demo_deepseek_chat.scm

(import (liii http)
  (liii json)
  (liii list)
  (liii logging)
  (liii os)
  (liii path)
  (liii string)
  (scheme base)
  (scheme file)
  (scheme write)
) ;import

(define config-file (string-append (getenv "HOME") "/.deepseek.json"))

;; 日志写入 /tmp/deepseek.log
(log-set-file-handler! "/tmp/deepseek.log")

;; 配置记录：api-key / base-url / model

(define default-base-url "https://api.deepseek.com")

(define default-model "deepseek-chat")

(define-record-type config
  (make-config api-key base-url model)
  config?
  (api-key config-api-key)
  (base-url config-base-url)
  (model config-model)
) ;define-record-type

;; 从配置文件读取配置记录，文件不存在或缺少 api_key 时返回 #f

(define (load-config)
  ;; 注意：goldfish 的 guard 在 body 正常返回时也会落入 else 子句，这里用 catch
  (catch #t
    (lambda ()
      (let* ((j (string->json (path-read-text config-file)))
             (api-key (json-ref j "api_key"))
             (base-url (json-ref-string j "base_url" default-base-url))
             (model (json-ref-string j "model" default-model))
            ) ;
        (if (string? api-key) (make-config api-key base-url model) #f)
      ) ;let*
    ) ;lambda
    (lambda (type info) #f)
  ) ;catch
) ;define

;; 流式解析状态记录

(define-record-type stream-state
  (%make-stream-state pending out-buffer out-char-count chunk-char-count
    full-response
  ) ;%make-stream-state
  stream-state?
  ;; SSE 解析：上次回调遗留的不完整行
  (pending stream-state-pending stream-state-pending-set!)
  ;; 输出缓冲：积累内容达到 100 个字符（UTF-8）就 flush 一次到终端
  (out-buffer stream-state-out-buffer stream-state-out-buffer-set!)
  (out-char-count stream-state-out-char-count stream-state-out-char-count-set!)
  ;; 日志计数：每累计 100 个字符（UTF-8）打印一条日志
  (chunk-char-count stream-state-chunk-char-count
    stream-state-chunk-char-count-set!
  ) ;chunk-char-count
  ;; 本次请求的完整回答（用于追加到对话历史）
  (full-response stream-state-full-response stream-state-full-response-set!)
) ;define-record-type

(define (make-stream-state)
  (%make-stream-state "" "" 0 0 "")
) ;define

(define state (make-stream-state))

;; 重置为初始状态：直接重建，初始值只在 make-stream-state 维护一份

(define (stream-state-reset!)
  (set! state (make-stream-state))
) ;define

(define (flush-buffer)
  (display (stream-state-out-buffer state))
  (stream-state-out-buffer-set! state "")
  (stream-state-out-char-count-set! state 0)
) ;define

;; 收到一段 delta 内容：积累，字符数达到 100 就 flush

(define (accumulate-content delta)
  (stream-state-full-response-set! state
    (string-append (stream-state-full-response state) delta)
  ) ;stream-state-full-response-set!
  (stream-state-out-buffer-set! state
    (string-append (stream-state-out-buffer state) delta)
  ) ;stream-state-out-buffer-set!
  (stream-state-out-char-count-set! state
    (+ (stream-state-out-char-count state) (utf8-string-length delta))
  ) ;stream-state-out-char-count-set!
  (if (>= (stream-state-out-char-count state) 100) (flush-buffer) #f)
) ;define

;; 解析一行 SSE: "data: {...}" 或 "data: [DONE]"

(define (handle-line line)
  (let ((line (string-trim-right line)))
    (if (string-starts? line "data: ")
      (let ((payload (string-remove-prefix line "data: ")))
        (if (string=? payload "[DONE]")
          #f
          (guard (err (else #f))
            (let* ((j (string->json payload))
                   (choices (json-ref j "choices"))
                   (delta (if (vector? choices) (json-ref (vector-ref choices 0) "delta") #f))
                  ) ;
              (if (json-object? delta)
                (let ((content (json-ref delta "content")))
                  (if (string? content) (accumulate-content content) #f)
                ) ;let
                #f
              ) ;if
            ) ;let*
          ) ;guard
        ) ;if
      ) ;let
      #f
    ) ;if
  ) ;let
) ;define

;; http-post stream 模式的回调

(define (on-chunk chunk)
  (stream-state-chunk-char-count-set! state
    (+ (stream-state-chunk-char-count state) (utf8-string-length chunk))
  ) ;stream-state-chunk-char-count-set!
  (when (>= (stream-state-chunk-char-count state) 100)
    (log-info "on-chunk: accumulated %(len)d chars, latest chunk: %(chunk)s"
      'len
      (stream-state-chunk-char-count state)
      'chunk
      chunk
    ) ;log-info
    (log-flush!)
    (stream-state-chunk-char-count-set! state 0)
  ) ;when
  (let* ((all (string-append (stream-state-pending state) chunk))
         (lines (string-split all "\n"))
         (ends-nl (string-ends? all "\n"))
         (complete (if ends-nl lines (drop-right lines 1)))
         (rest (if ends-nl "" (last lines)))
        ) ;
    (stream-state-pending-set! state rest)
    (for-each handle-line complete)
    #t
  ) ;let*
) ;define

;; 消息与对话历史：message 为 (("role" . r) ("content" . c)) 结构的 alist，
;; conversation-history 保存全量历史，新消息在前（时间倒序）

(define (make-message role content)
  (list (cons "role" role) (cons "content" content))
) ;define

(define conversation-history '())

(define (history-add! msg)
  (set! conversation-history (cons msg conversation-history))
) ;define

;; 聊天请求记录：model / messages（按时间顺序的消息列表）

(define-record-type chat-request
  (make-chat-request model messages)
  chat-request?
  (model chat-request-model)
  (messages chat-request-messages)
) ;define-record-type

;; 转为 (liii json) 可序列化的结构（alist 表示对象，vector 表示数组）
;; thinking.type=disabled 关闭思考模式（v4 系列模型默认开启，开启时会返回 reasoning_content）
;; 注：不用 quasiquote，S7 的 quasiquote 无法正确处理 ("key" . ,value) 形式的点对

(define (chat-request->json req)
  (list (cons "model" (chat-request-model req))
    (cons "messages" (list->vector (chat-request-messages req)))
    (cons "thinking" (list (cons "type" "disabled")))
    (cons "stream" #t)
  ) ;list
) ;define

(define (ask cfg question)
  (stream-state-reset!)
  (let* ((url (string-append (config-base-url cfg) "/chat/completions"))
         ;; 全量历史 + 本次问题；请求成功后才提交到历史，失败时历史保持不变
         (messages (reverse (cons (make-message "user" question) conversation-history)))
         (req (make-chat-request (config-model cfg) messages))
         (body (json->string (chat-request->json req)))
        ) ;
    ;; 关键日志：请求 URL 与完整请求 body
    (log-info "request url: %(url)s" 'url url)
    (log-info "request body: %(body)s" 'body body)
    (http-post url
      :data
      body
      :headers
      (list (cons "Content-Type" "application/json")
        (cons "Authorization" (string-append "Bearer " (config-api-key cfg)))
      ) ;list
      :stream
      #t
      :callback
      on-chunk
    ) ;http-post
    (log-info "request done")
  ) ;let*
  (history-add! (make-message "user" question))
  (history-add! (make-message "assistant" (stream-state-full-response state)))
  (flush-buffer)
  (newline)
) ;define

;; 脚本模式下 (read-line) 无法读到终端输入，须显式打开 /dev/stdin

(define stdin-port (open-input-file "/dev/stdin"))

;; 读取一行；Ctrl+D（EOF）时返回 #f，空行返回 ""

(define (prompt-line prompt)
  (display prompt)
  ;; 必须显式 flush，否则提示语滞留在 stdout 缓冲区，
  ;; read-line 阻塞时终端一片空白，看起来像程序卡死
  (flush-output-port)
  (let ((line (read-line stdin-port)))
    (if (eof-object? line) #f (string-trim line))
  ) ;let
) ;define

;; utf8-string-length 遇到非法 UTF-8 字节会抛 value-error，借此校验输入

(define (valid-utf8-string? s)
  (catch #t (lambda () (utf8-string-length s) #t) (lambda (type info) #f))
) ;define

(define (chat-loop cfg)
  (let loop
    ()
    (let ((question (prompt-line "\nQ> ")))
      (cond ((not question) (display "再见！\n"))
            ((or (string=? question "quit") (string=? question "exit"))
             (display "再见！\n")
            ) ;
            ((string-null? question) (loop))
            ((not (valid-utf8-string? question))
             (display "输入不是合法的 UTF-8 字符串，未发起请求。\n")
             (loop)
            ) ;
            (else (display "A> ")
              (flush-output-port)
              ;; goldfish 的 guard 在正常返回时也会落入 else 子句，故用 catch
              (catch #t
                (lambda () (ask cfg question))
                (lambda (type info)
                  (log-error "request failed: %(err)s" 'err info)
                  (display "请求失败：")
                  (write info)
                  (newline)
                ) ;lambda
              ) ;catch
              (loop)
            ) ;else
      ) ;cond
    ) ;let
  ) ;let
) ;define

;; 加载配置；缺失时提示手动输入 API Key，构造仅含 key 的默认配置

(define (prompt-config)
  (let ((key (prompt-line "未找到 ~/.deepseek.json，请手动输入 DeepSeek API Key: ")
        ) ;key
       ) ;
    (if (or (not key) (string-null? key))
      #f
      (make-config key default-base-url default-model)
    ) ;if
  ) ;let
) ;define

(define config (or (load-config) (prompt-config)))

(if config
  (begin
    (log-info "config ready, model: %(model)s" 'model (config-model config))
    (chat-loop config)
  ) ;begin
  (begin
    (log-info "config not found at %(path)s" 'path config-file)
    (display "未输入 API Key，退出。\n")
  ) ;begin
) ;if
