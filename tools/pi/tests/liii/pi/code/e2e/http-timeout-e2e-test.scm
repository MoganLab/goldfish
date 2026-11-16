;; http-timeout-e2e
;; 本地 stub 服务端驱动的 HTTP 超时模型端到端测试：
;; 1. 长流持续输出超过空闲窗口时不被总时限掐断；
;; 2. 服务端停顿时按空闲超时中断并保留已产生部分输出；
;; 3. 空闲超时可自动重试并最终恢复；
;; 4. httpIdleTimeoutMs 设为 "disabled" 时关闭空闲超时。
;;
;; 语法
;; ----
;; (pi-request-provider-stream provider config messages system-prompt tools on-event)
;;
;; 参数
;; ----
;; provider : string?，供应商标识。
;; config : list?，由 pi-make-request-config 构造的请求配置（含 settings）。
;; messages : list?，消息列表。
;; system-prompt : (or string? #f)，系统提示词。
;; tools : list?，工具定义列表。
;; on-event : (or procedure? #f)，流式事件回调。
;;
;; 返回值
;; ----
;; assistant message
;; 返回携带 stop-reason、content 与 error-message 的助手消息。

(import (scheme base)
  (liii check)
  (liii string)
  (liii subprocess)
  (liii pi ai config record)
  (liii pi ai dispatch)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define (read-stub-port proc)
  (let loop
    ((n 0) (port #f))
    (cond
     ((and (not port) (< n 20))
      (loop (+ n 1) (parse-port-line (process-read-line proc)))
     ) ;
     (else port)
    ) ;cond
  ) ;let
) ;define

(define stub-py
  (string-append "import http.server, json, time, sys\n"
    "state = {'retry_calls': 0}\n" "def sse_chunk(text, finish=None):\n"
    "    obj = {'id':'1','object':'chat.completion.chunk','choices':[{'index':0,'delta':{'content':text} if text else {},'finish_reason':finish}]}\n"
    "    return ('data: ' + json.dumps(obj) + '\\n\\n').encode()\n"
    "class H(http.server.BaseHTTPRequestHandler):\n"
    "    def log_message(self, *a): pass\n" "    def do_POST(self):\n"
    "        n = int(self.headers.get('Content-Length', '0'))\n"
    "        if n > 0: self.rfile.read(n)\n" "        self.send_response(200)\n"
    "        self.send_header('Content-Type', 'text/event-stream')\n"
    "        self.end_headers()\n" "        try:\n"
    "            if self.path.startswith('/long-stream'):\n"
    "                for i in range(6):\n"
    "                    self.wfile.write(sse_chunk(f'c{i}-'))\n"
    "                    self.wfile.flush()\n"
    "                    time.sleep(0.35)\n"
    "                self.wfile.write(sse_chunk('', 'stop'))\n"
    "                self.wfile.write(b'data: [DONE]\\n\\n')\n"
    "                self.wfile.flush()\n"
    "            elif self.path.startswith('/stall-only'):\n"
    "                self.wfile.write(sse_chunk('partial-before-stall'))\n"
    "                self.wfile.flush()\n" "                time.sleep(4.0)\n"
    "            elif self.path.startswith('/stall-then-ok'):\n"
    "                state['retry_calls'] += 1\n"
    "                if state['retry_calls'] == 1:\n"
    "                    self.wfile.write(sse_chunk('first-part'))\n"
    "                    self.wfile.flush()\n"
    "                    time.sleep(4.0)\n" "                else:\n"
    "                    self.wfile.write(sse_chunk('recovered-ok'))\n"
    "                    self.wfile.write(sse_chunk('', 'stop'))\n"
    "                    self.wfile.write(b'data: [DONE]\\n\\n')\n"
    "                    self.wfile.flush()\n"
    "            elif self.path.startswith('/slow-pause'):\n"
    "                self.wfile.write(sse_chunk('before-pause-'))\n"
    "                self.wfile.flush()\n" "                time.sleep(1.5)\n"
    "                self.wfile.write(sse_chunk('after-pause'))\n"
    "                self.wfile.write(sse_chunk('', 'stop'))\n"
    "                self.wfile.write(b'data: [DONE]\\n\\n')\n"
    "                self.wfile.flush()\n" "        except BrokenPipeError:\n"
    "            pass\n"
    "srv = http.server.ThreadingHTTPServer(('127.0.0.1', 0), H)\n"
    "print(f'PORT:{srv.server_address[1]}', flush=True)\n"
    "srv.serve_forever()\n"
  ) ;string-append
) ;define

(define (make-cfg port path idle-ms provider-max-retries)
  (let* ((settings (pi-settings
                     :http-idle-timeout-ms idle-ms
                     :retry
                     (pi-retry-config
                       :enabled              #t
                       :base-delay-ms        10
                       :max-agent-delay-ms   50
                       :provider-max-retries provider-max-retries
                     ) ;pi-retry-config
                   ) ;pi-settings
         ) ;settings
        ) ;
    (pi-make-request-config "openai"
      "gpt-4o-mini"
      "sk-test"
      :base-url (string-append "http://127.0.0.1:" (number->string port) path)
      :provider-api "openai-completions"
      :settings     settings
    ) ;pi-make-request-config
  ) ;let*
) ;define

(let* ((proc (process-spawn (list "python3" "-u" "-c" stub-py)
               :stdin  'null
               :stdout 'pipe
               :stderr 'discard
             ) ;process-spawn
       ) ;proc
       (port (read-stub-port proc))
       (msgs (list (pi-user-message "hi")))
      ) ;
  (check-true (number? port))
  (dynamic-wind (lambda () #f)
    (lambda ()
      ;; 1. 长流持续输出（总耗时 ~2.1s > 空闲窗口 1.0s）：不被总时限掐断
      (let* ((cfg (make-cfg port "/long-stream" 1000 0))
             (resp (pi-request-provider-stream "openai" cfg msgs #f '() #f))
             (txt (pi-text-block-text (car (pi-assistant-message-content resp))))
            ) ;
        (check (pi-assistant-message-stop-reason resp) => "stop")
        (check txt => "c0-c1-c2-c3-c4-c5-")
      ) ;let*

      ;; 2. 服务端停摆触发空闲超时，且保留已收到的部分输出
      (let* ((cfg (make-cfg port "/stall-only" 1000 0))
             (resp (pi-request-provider-stream "openai" cfg msgs #f '() #f))
             (blocks (pi-assistant-message-content resp))
             (err (pi-assistant-message-error-message resp))
            ) ;
        (check (pi-assistant-message-stop-reason resp) => "error")
        (check-true (string-contains? (or err "") "timed out"))
        (check-true (pair? blocks))
        (check (pi-text-block-text (car blocks)) => "partial-before-stall")
      ) ;let*

      ;; 3. 空闲超时后自动重试并最终成功
      (let* ((cfg (make-cfg port "/stall-then-ok" 1000 1))
             (resp (pi-request-provider-stream "openai" cfg msgs #f '() #f))
             (txt (pi-text-block-text (car (pi-assistant-message-content resp))))
            ) ;
        (check (pi-assistant-message-stop-reason resp) => "stop")
        (check txt => "recovered-ok")
      ) ;let*

      ;; 4. httpIdleTimeoutMs 设为 "disabled" 时关闭空闲超时，1.5s 停顿不中断
      (let* ((cfg (make-cfg port "/slow-pause" "disabled" 0))
             (resp (pi-request-provider-stream "openai" cfg msgs #f '() #f))
             (txt (pi-text-block-text (car (pi-assistant-message-content resp))))
            ) ;
        (check (pi-assistant-message-stop-reason resp) => "stop")
        (check txt => "before-pause-after-pause")
      ) ;let*
    ) ;lambda
    (lambda () (process-kill proc) (process-wait proc))
  ) ;dynamic-wind
) ;let*

(check-report)
