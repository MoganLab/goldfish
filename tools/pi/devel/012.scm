(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi args)
  (liii pi config)
  (liii pi message)
  (liii pi event)
  (liii pi gemini thinking)
  (liii pi anthropic thinking)
  (liii pi modes)
) ;import

(check-set-mode! 'report-failed)

;; ==========================================
;; [012] thinking 内容 验收脚本
;; ==========================================

(display "=== 验收点 1: --thinking-level 解析与校验（非法值退出码 1）===\n"
) ;display

;; 1.1 参数解析合法值 (off / low / medium / high)
(let ((r1 (parse-pi-args '("--thinking-level" "off")))
      (r2 (parse-pi-args '("--thinking-level" "low")))
      (r3 (parse-pi-args '("--thinking-level" "medium")))
      (r4 (parse-pi-args '("--thinking-level" "high")))
      (r5 (parse-pi-args '("--thinking" "low")))
     ) ;
  (check (pi-args-ref r1 'thinking-level) => "off")
  (check (pi-args-ref r2 'thinking-level) => "low")
  (check (pi-args-ref r3 'thinking-level) => "medium")
  (check (pi-args-ref r4 'thinking-level) => "high")
  (check (pi-args-ref r5 'thinking-level) => "low")
) ;let

;; 1.2 参数解析非法值抛出 pi-arg-error
(check-catch 'pi-arg-error (parse-pi-args '("--thinking-level" "invalid")))
(check-catch 'pi-arg-error (parse-pi-args '("--thinking" "super-high")))

;; 1.3 CLI 运行非法值返回退出码 1
(check (run "bin/gf pi --thinking-level invalid \"hello\"" :timeout 15) => 1)
(check (run "bin/gf pi --thinking extreme \"hello\"" :timeout 15) => 1)

(display "=== 验收点 2: Gemini thinkingConfig 注入断言 ===\n")

;; 2.1 映射与构造
(check (gemini-thinking-level->google-level "low") => "LOW")
(check (gemini-thinking-level->google-level "medium") => "MEDIUM")
(check (gemini-thinking-level->google-level "high") => "HIGH")
(check (gemini-thinking-level->google-level "off") => #f)

(check (gemini-build-thinking-config "off") => #f)
(check (gemini-build-thinking-config #f) => #f)

(let ((cfg (gemini-build-thinking-config "low")))
  (check-true (pair? cfg))
  (let ((tc (cdr (assoc "thinkingConfig" cfg))))
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "LOW")
  ) ;let
) ;let

;; 2.2 请求体注入断言
(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "medium"))
      ) ;
  (check-true (pair? (assoc "generationConfig" injected)))
  (let* ((gc (cdr (assoc "generationConfig" injected)))
         (tc (cdr (assoc "thinkingConfig" gc)))
        ) ;
    (check (cdr (assoc "includeThoughts" tc)) => #t)
    (check (cdr (assoc "thinkingLevel" tc)) => "MEDIUM")
  ) ;let*
) ;let*

(let* ((base '(("contents" . #())))
       (injected (gemini-inject-thinking-config base "off"))
      ) ;
  (check-false (assoc "generationConfig" injected))
  (check injected => base)
) ;let*

(display "=== 验收点 3: Anthropic thinking block 注入断言 ===\n")

;; 3.1 预算映射
(check (anthropic-thinking-level->budget "low") => 1024)
(check (anthropic-thinking-level->budget "medium") => 4096)
(check (anthropic-thinking-level->budget "high") => 8192)
(check (anthropic-thinking-level->budget "off") => #f)

;; 3.2 注入请求体断言（off 不注入，low/high 注入并按需适配 max_tokens）
(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "off"))
      ) ;
  (check-false (assoc "thinking" res))
) ;let*

(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 8192)))
       (res (anthropic-inject-thinking body "low"))
      ) ;
  (check-true (pair? (assoc "thinking" res)))
  (let ((th (cdr (assoc "thinking" res))))
    (check (cdr (assoc "type" th)) => "enabled")
    (check (cdr (assoc "budget_tokens" th)) => 1024)
  ) ;let
  (check (cdr (assoc "max_tokens" res)) => 8192)
) ;let*

(let* ((body '(("model" . "claude-3-7-sonnet") ("max_tokens" . 4096)))
       (res (anthropic-inject-thinking body "high"))
      ) ;
  (check (cdr (assoc "max_tokens" res)) => (+ 8192 4096))
) ;let*

(display "=== 验收点 4: print 模式只输出 text 块（忽略 thinking 块）===\n"
) ;display

;; 4.1 handle-print-result 处理混合 message（含 thinking 与 text）时正常返回退出码 0 且只输出 text
(let* ((mixed-msg (pi-assistant-message (list (pi-thinking-block "private internal thinking" "sig-xyz")
                                          (pi-text-block "visible final answer")
                                        ) ;list
                    :stop-reason "stop"
                  ) ;pi-assistant-message
       ) ;mixed-msg
       (out
         (with-output-to-string
           (lambda () (let ((ret (handle-print-result mixed-msg))) (check ret => 0)))
         ) ;with-output-to-string
       ) ;out
      ) ;
  (check-true (string-contains? out "visible final answer"))
  (check-false (string-contains? out "private internal thinking"))
) ;let*

(display "=== 验收点 5: JSON 事件流含 thinking 增量 ===\n")

;; 5.1 Gemini 流式 thinking 状态机与事件触发
(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (gemini-append-thinking-block! stream-state "Thinking step 1" "sig-1" emit)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (check (cdr (assoc 'type (cadr events))) => "thinking_delta")
  (check (cdr (assoc 'delta (cadr events))) => "Thinking step 1")
  (gemini-stream-stop-active-block! stream-state emit)
  (check (cdr (assoc 'type (list-ref events 2))) => "thinking_end")
  (check (cdr (assoc 'content (list-ref events 2))) => "Thinking step 1")
) ;let*

;; 5.2 Anthropic 流式 thinking 状态机与事件触发
(let* ((stream-state (vector '() #f '((input . 10) (output . 5)) '() #f #f #f))
       (events '())
       (emit
         (lambda (ev) (set! events (append events (list ev))))
       ) ;emit
      ) ;
  (anthropic-start-thinking-block stream-state 0 emit)
  (check (cdr (assoc 'type (car events))) => "thinking_start")
  (anthropic-handle-thinking-delta '(("type" . "thinking_delta")
                                     ("thinking" . "Anthropic step 1"))
    0
    stream-state
    emit
  ) ;anthropic-handle-thinking-delta
  (check (cdr (assoc 'type (cadr events))) => "thinking_delta")
  (check (cdr (assoc 'delta (cadr events))) => "Anthropic step 1")
  (anthropic-finish-thinking-block stream-state emit)
  (check (cdr (assoc 'type (caddr events))) => "thinking_end")
  (check (cdr (assoc 'content (caddr events))) => "Anthropic step 1")
) ;let*

;; 5.3 JSON 模式序列化事件验证
(let* ((ev-data (list (cons 'type "thinking_delta")
                  (cons 'contentIndex 0)
                  (cons 'delta "think")
                ) ;list
       ) ;ev-data
       (ev-json
         (pi-serialize-event
           (pi-build-json-event 'message_update (list '((input . 1)
                                                        (output . 1)) ev-data))
         ) ;pi-serialize-event
       ) ;ev-json
      ) ;
  (check-true (string-contains? ev-json "\"type\":\"message_update\""))
  (check-true (string-contains? ev-json "\"type\":\"thinking_delta\""))
  (check-true (string-contains? ev-json "\"delta\":\"think\""))
) ;let*

(display "=== 验收点 6: bin/gf test 全量单元测试套件通过 ===\n")
(check (run "bin/gf test tools/pi/tests/liii/" :timeout 120) => 0)

(display "=== 验收点 7: 真实冒烟 --thinking-level low 正常返回 ===\n"
) ;display

;; 7.1 print 模式带 --thinking-level low 正常返回
(let* ((tmp-out "/tmp/pi-012-smoke-print.txt")
       (ret (run (string-append "sh -c 'bin/gf pi -p --thinking-level low \"计算 100 + 200 等于几？\" > "
                   tmp-out
                   " 2>&1'"
                 ) ;string-append
              :timeout 60
            ) ;run
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "300"))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

;; 7.2 JSON 模式带 --thinking-level low 正常返回
(let* ((tmp-out "/tmp/pi-012-smoke-json.txt")
       (ret (run (string-append "sh -c 'bin/gf pi --mode json --thinking-level low \"计算 100 + 200 等于几？\" > "
                   tmp-out
                   " 2>&1'"
                 ) ;string-append
              :timeout 60
            ) ;run
       ) ;ret
       (out (if (file-exists? tmp-out) (path-read-text tmp-out) ""))
      ) ;
  (check ret => 0)
  (check-true (string-contains? out "\"type\":\"session\""))
  (check-true (string-contains? out "\"type\":\"agent_settled\""))
  (when (file-exists? tmp-out)
    (delete-file tmp-out)
  ) ;when
) ;let*

(display "=== 验收点 8: 门禁检查通过 ===\n")
(check (run "timeout 600 xmake b goldfish" :timeout 650) => 0)
(check (run "bin/gf fmt" :timeout 120) => 0)
(check (run "bin/gf check" :timeout 120) => 0)

(check-report)
