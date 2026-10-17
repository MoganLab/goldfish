;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(set! *load-path*
  (append (list "tools/pi" ".") *load-path*))

(import (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii subprocess)
  (liii json)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tests/stub_gemini_server.py")
    "tests/stub_gemini_server.py"
    (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
      "tools/pi/tests/stub_gemini_server.py"
      "../../tools/pi/tests/stub_gemini_server.py"
    ) ;if
  ) ;if
) ;define

(define FIXTURE_DIR
  (if (file-exists? "tests/fixtures/direct")
    "tests/fixtures/direct"
    (if (file-exists? "tools/pi/tests/fixtures/direct")
      "tools/pi/tests/fixtures/direct"
      "../../tools/pi/tests/fixtures/direct"
    ) ;if
  ) ;if
) ;define

(define GF_BIN
  (if (file-exists? "bin/gf")
    "bin/gf"
    (if (file-exists? "../../bin/gf") "../../bin/gf" "bin/gf")
  ) ;if
) ;define

(let* ((proc (process-spawn (list "python3" stub-server-file) :stdout 'pipe))
       (port-line (process-read-line proc))
       (port (parse-port-line port-line))
       (base-url (string-append "http://127.0.0.1:" (number->string port)))
       (test-env (list (cons "PI_CODING_AGENT_DIR" FIXTURE_DIR)
                       (cons "GF_PI_BASE_URL" base-url)
                       (cons "GF_PI_MODEL" "gemini-3.8-flash")
                       (cons "https_proxy" "")
                       (cons "http_proxy" "")
                       (cons "all_proxy" "")
                       (cons "ALL_PROXY" "")
                       (cons "HTTPS_PROXY" "")
                       (cons "HTTP_PROXY" "")))
      ) ;
  (check-true (number? port))

  ;; 1. 单轮简单对话的 JSONL 事件序列
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "Hello from json e2e test"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (raw-out (process-read p :stream 'stdout))
         (lines (filter (lambda (s) (not (string-null? s))) (string-split raw-out "\n")))
         (events (map string->json lines))
         (event-types (map (lambda (ev) (cdr (assoc "type" ev))) events))
        ) ;
    (check code => 0)
    (check-true (>= (length lines) 8))
    ;; 第一条记录必须是 session header (version 3)
    (let ((header (car events)))
      (check (cdr (assoc "type" header)) => "session")
      (check (cdr (assoc "version" header)) => 3)
      (check-true (string? (cdr (assoc "id" header))))
      (check-true (string? (cdr (assoc "cwd" header))))
    ) ;let
    ;; 验证基础运行的事件类型序列（流式模式下包含 message_update）
    (check-true (pair? (member "message_update" event-types)))
    (check (car event-types) => "session")
    (check (cadr event-types) => "agent_start")
    (check (caddr event-types) => "turn_start")
    (check (car (reverse event-types)) => "agent_settled")
    (process-close p))

  ;; 2. 多轮带工具调用（functionCall）的 JSONL 事件序列
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "--model" "multi-turn" "Run multi turn steps"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 15000))
         (raw-out (process-read p :stream 'stdout))
         (lines (filter (lambda (s) (not (string-null? s))) (string-split raw-out "\n")))
         (events (map string->json lines))
         (event-types (map (lambda (ev) (cdr (assoc "type" ev))) events))
        ) ;
    (check code => 0)
    ;; 验证 jq 逐行可解析
    (let* ((jq-p (process-spawn (list "jq" "-c" ".") :stdin 'pipe :stdout 'pipe))
           (_ (process-write jq-p raw-out))
           (_ (process-close-stdin jq-p))
           (jq-code (process-wait jq-p 5000))
          ) ;
      (check jq-code => 0)
      (process-close jq-p)
    ) ;let*
    ;; 检查关键事件包含 tool_execution_start, tool_execution_end
    (check-true (pair? (member "tool_execution_start" event-types)))
    (check-true (pair? (member "tool_execution_end" event-types)))
    (check (car event-types) => "session")
    (check (car (reverse event-types)) => "agent_settled")
    (process-close p))

  ;; 3. 模型报错/aborted 情况下进程退出码仍为 0（§3.3：事件流中出现 error/aborted 不改变退出码）
  (let* ((cmd (list GF_BIN "pi" "--mode" "json" "--model" "safety" "Run safety check"))
         (p (process-spawn cmd :env test-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (raw-out (process-read p :stream 'stdout))
         (lines (filter (lambda (s) (not (string-null? s))) (string-split raw-out "\n")))
         (events (map string->json lines))
         (event-types (map (lambda (ev) (cdr (assoc "type" ev))) events))
        ) ;
    (check code => 0)
    (check-true (pair? (member "session" event-types)))
    (check-true (pair? (member "agent_settled" event-types)))
    (process-close p))

  ;; 4. Anthropic provider JSON 模式事件流验证
  (let* ((ant-env (append (list (cons "ANTHROPIC_API_KEY" "sk-ant-test-json")) test-env))
         (cmd (list GF_BIN "pi" "--provider" "anthropic" "--mode" "json" "Hello anthropic json"))
         (p (process-spawn cmd :env ant-env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p 10000))
         (raw-out (process-read p :stream 'stdout))
         (lines (filter (lambda (s) (not (string-null? s))) (string-split raw-out "\n")))
         (events (map string->json lines))
         (event-types (map (lambda (ev) (cdr (assoc "type" ev))) events))
        ) ;
    (check code => 0)
    (check (car event-types) => "session")
    (check (car (reverse event-types)) => "agent_settled")
    (check-true (pair? (member "message_update" event-types)))
    (process-close p))

  (process-kill proc)
) ;let*

(check-report)
