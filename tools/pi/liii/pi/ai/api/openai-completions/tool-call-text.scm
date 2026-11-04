;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; OpenAI 兼容端点里"模型把工具调用写在正文里"的兜底解析：
;; 一些本地模型（Qwen / DeepSeek-Coder 等）不会返回结构化的 tool_calls，
;; 而是把调用以 XML 标签或 JSON 片段的形式直接写进 assistant 文本。
;; 本模块把这类正文还原成标准的 (name . args) 工具调用描述，
;; 供 stream-parse 在流结束时补齐 toolCall 块。
(define-library (liii pi ai api openai-completions tool-call-text)
  (import (scheme base)
    (liii string)
    (liii json)
    (liii pi ai message)
    (liii pi ai api openai-completions stream-state)
  ) ;import
  (export extract-openai-json-snippet parse-openai-xml-tool-call
    parse-openai-text-tool-call maybe-extract-openai-text-tool-call!
  ) ;export
  (begin

    ;; XML 工具调用标签的起始偏移量（标签本身含 11 个字符）
    (define %openai-tool-call-open-len 11)

    ;; alist 取键（字符串键或符号键均可），与 (liii pi code config user-fs)
    ;; 的 pi-user-dict-ref 语义一致；此处本地实现以避免 ai 层依赖 code 层
    (define (%dict-ref obj key default-val)
      (if (not (pair? obj))
        default-val
        (let* ((str-key (if (symbol? key) (symbol->string key) key))
               (sym-key (if (string? key) (string->symbol key) key))
               (pair (or (assoc str-key obj) (assoc sym-key obj)))
              ) ;
          (if pair (cdr pair) default-val)
        ) ;let*
      ) ;if
    ) ;define

    ;; 安全地把字符串解析为 JSON alist，失败返回 #f
    (define (%safe-json text)
      (catch #t (lambda () (string->json (string-trim-both text))) (lambda args #f))
    ) ;define

    ;; 从正文里截取可能的 JSON 片段：
    ;; 1) 显式 XML 工具调用标签包裹的片段；
    ;; 2) json 代码块 / 通用代码块；
    ;; 3) 含 name 字段的最短片段。
    (define (extract-openai-json-snippet text)
      (let* ((trimmed (string-trim-both text))
             (tc-start (string-position "<tool_call>" trimmed))
             (tc-end (string-position "</tool_call>" trimmed))
            ) ;
        (cond
         ((and tc-start tc-end (< tc-start tc-end))
          (string-trim-both (substring trimmed (+ tc-start %openai-tool-call-open-len) tc-end)
          ) ;string-trim-both
         ) ;
         ((string-starts? trimmed "```json") (%strip-code-fence trimmed 7))
         ((string-starts? trimmed "```") (%strip-code-fence trimmed 3))
         (else (%extract-named-json trimmed))
        ) ;cond
      ) ;let*
    ) ;define

    ;; 去掉代码块围栏，返回围栏内的正文
    (define (%strip-code-fence trimmed prefix-len)
      (let* ((rest (string-trim-both (substring trimmed prefix-len (string-length trimmed))))
             (end (string-position "```" rest))
            ) ;
        (if end (string-trim-both (substring rest 0 end)) rest)
      ) ;let*
    ) ;define

    ;; 截取从 name 字段起始到最后一个右花括号的片段
    (define (%extract-named-json trimmed)
      (let* ((b-start (or (string-position "{\"name\":" trimmed)
                        (string-position "{\"name\" :" trimmed)
                        (string-position "{\"name\"  :" trimmed)
                      ) ;or
             ) ;b-start
            ) ;
        (if b-start
          (let ((r-idx (string-index-right trimmed #\})))
            (if (and r-idx (>= r-idx b-start))
              (substring trimmed b-start (+ r-idx 1))
              trimmed
            ) ;if
          ) ;let
          trimmed
        ) ;if
      ) ;let*
    ) ;define

    ;; 解析 parameter 标签列表，返回 ((name . value) ...)
    (define (%parse-xml-parameters inner pos)
      (let loop
        ((pos pos) (params '()))
        (let ((p-start (string-position "<parameter=" inner pos)))
          (if (not p-start)
            (reverse params)
            (%parse-xml-parameter inner p-start params loop)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 解析单个 parameter 片段，解析成功后继续处理后续片段
    (define (%parse-xml-parameter inner p-start params loop)
      (let* ((p-name-start (+ p-start 11))
             (p-name-end (string-position ">" inner p-name-start))
            ) ;
        (if (not p-name-end)
          (reverse params)
          (let* ((p-name (string-trim-both (substring inner p-name-start p-name-end)))
                 (val-start (+ p-name-end 1))
                 (val-end (string-position "</parameter>" inner val-start))
                ) ;
            (if (not val-end)
              (reverse params)
              (let* ((val-raw (string-trim-both (substring inner val-start val-end)))
                     (num-val (string->number val-raw))
                     (val (if (number? num-val) num-val val-raw))
                    ) ;
                (loop (+ val-end 12) (cons (cons p-name val) params))
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; 解析 function 标签形式的工具调用（参数在 parameter 标签里）
    (define (%parse-xml-function-call inner)
      (let ((fn-start (string-position "<function=" inner)))
        (if (not fn-start)
          #f
          (let* ((after-fn (+ fn-start 10)) (fn-end (string-position ">" inner after-fn)))
            (if (not fn-end)
              #f
              (let* ((fn-name (string-trim-both (substring inner after-fn fn-end)))
                     (args (%parse-xml-parameters inner fn-end))
                    ) ;
                (cons fn-name args)
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 解析 XML 工具调用标签内的内容：
    ;; 优先当作 JSON（name + arguments），否则按 function 标签形式解析
    (define (%parse-xml-tool-call-body inner)
      (let ((json-parsed (%safe-json inner)))
        (if (and (pair? json-parsed) (assoc "name" json-parsed))
          (let* ((name (cdr (assoc "name" json-parsed)))
                 (raw-args (or (%dict-ref json-parsed "arguments" #f)
                             (%dict-ref json-parsed "parameters" '())
                           ) ;or
                 ) ;raw-args
                ) ;
            (cons name raw-args)
          ) ;let*
          (%parse-xml-function-call inner)
        ) ;if
      ) ;let
    ) ;define

    ;; 从正文里解析 XML 形式的工具调用，非 XML 形式返回 #f
    (define (parse-openai-xml-tool-call text)
      (let* ((tc-start (string-position "<tool_call>" text))
             (tc-end (string-position "</tool_call>" text))
            ) ;
        (if (and tc-start tc-end (< tc-start tc-end))
          (let ((inner (substring text (+ tc-start %openai-tool-call-open-len) tc-end)))
            (%parse-xml-tool-call-body inner)
          ) ;let
          #f
        ) ;if
      ) ;let*
    ) ;define

    ;; 把工具调用的原始参数规整为 alist：
    ;; 已是 alist 直接用；是 JSON 字符串则解析；bash 归一为 command 参数
    (define (%normalize-tool-args name raw-args)
      (cond ((pair? raw-args) raw-args)
            ((string? raw-args)
             (let ((sub (%safe-json raw-args)))
               (cond ((pair? sub) sub)
                     ((equal? name "bash") (list (cons "command" raw-args)))
                     (else (list (cons "input" raw-args)))
               ) ;cond
             ) ;let
            ) ;
            (else '())
      ) ;cond
    ) ;define

    ;; 从任意正文里解析工具调用（先 XML 后 JSON），没有则返回 #f
    (define (parse-openai-text-tool-call text)
      (if (not (string? text))
        #f
        (let ((xml-call (parse-openai-xml-tool-call text)))
          (if xml-call xml-call (%parse-json-tool-call text))
        ) ;let
      ) ;if
    ) ;define

    ;; 从正文里解析 JSON 形式的工具调用
    (define (%parse-json-tool-call text)
      (let ((parsed (%safe-json (extract-openai-json-snippet text))))
        (if (and (pair? parsed) (assoc "name" parsed))
          (let* ((name (cdr (assoc "name" parsed)))
                 (raw-args
                   (let ((p (or (assoc "arguments" parsed) (assoc "parameters" parsed))))
                     (if p (cdr p) '())
                   ) ;let
                 ) ;raw-args
                ) ;
            (cons name (%normalize-tool-args name raw-args))
          ) ;let*
          #f
        ) ;if
      ) ;let
    ) ;define

    ;; 流结束时从正文里补齐内联工具调用：
    ;; 1) 文本或思考内容里能解析出工具调用时，写入 toolCall 块；
    ;; 2) 没有工具调用但只有思考内容时，把思考内容转成普通文本块。
    (define (maybe-extract-openai-text-tool-call! stream-state on-event)
      (let* ((blocks (openai-stream-state-blocks stream-state))
             (last-text (openai-stream-state-last-text stream-state))
             (last-reasoning (openai-stream-state-last-reasoning stream-state))
             (tc (or (parse-openai-text-tool-call last-text)
                   (parse-openai-text-tool-call last-reasoning)
                 ) ;or
             ) ;tc
            ) ;
        (cond (tc (%emit-inline-tool-call! stream-state tc on-event))
              ((and (null? blocks)
                 (string? last-reasoning)
                 (not (string-null? (string-trim-both last-reasoning)))
               ) ;and
               (%promote-reasoning-to-text! stream-state last-reasoning)
              ) ;
              (else #f)
        ) ;cond
      ) ;let*
    ) ;define

    ;; 把解析出的内联工具调用写入 blocks 并派发 toolcall_start/end
    (define (%emit-inline-tool-call! stream-state tc on-event)
      (let* ((name (car tc))
             (args (cdr tc))
             (call-id "call_inline_1")
             (tc-blk (pi-tool-call-block call-id name args))
             (usage (openai-stream-state-usage stream-state))
            ) ;
        (openai-stream-state-blocks-set! stream-state (list tc-blk))
        (openai-stream-state-stop-reason-set! stream-state "toolUse")
        (when (procedure? on-event)
          (on-event (list (cons 'type "toolcall_start")
                      (cons 'contentIndex 0)
                      (cons 'id call-id)
                      (cons 'toolName name)
                      (cons 'usage usage)
                    ) ;list
          ) ;on-event
          (on-event (list (cons 'type "toolcall_end")
                      (cons 'contentIndex 0)
                      (cons 'id call-id)
                      (cons 'toolName name)
                      (cons 'usage usage)
                    ) ;list
          ) ;on-event
        ) ;when
      ) ;let*
    ) ;define

    ;; 仅有思考内容时，把思考内容转成普通文本块
    (define (%promote-reasoning-to-text! stream-state last-reasoning)
      (let* ((clean-text (string-trim-both last-reasoning))
             (txt-blk (pi-text-block clean-text))
            ) ;
        (openai-stream-state-blocks-set! stream-state (list txt-blk))
        (openai-stream-state-last-text-set! stream-state clean-text)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
