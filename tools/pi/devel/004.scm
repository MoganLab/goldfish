(import (scheme base)
  (scheme file)
  (liii check)
  (liii subprocess)
  (liii json)
  (liii string)
  (liii list)
  (liii os)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; [004] record 化·事件结构 验收脚本
(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))

;; 断言点 1: 事件对象为 define-record-type（谓词与访问器可用）
(let* ((cwd "/tmp/test-pi")
       (sid "custom-id-001")
       (ts "2026-10-19T00:00:00.000Z")
       (sess (pi-event-session cwd sid ts))
       (a-start (pi-event-agent-start))
       (t-start (pi-event-turn-start))
       (u-msg '(("role" . "user") ("content" . "hi")))
       (m-start (pi-event-message-start u-msg))
       (usage '(("input" . 10) ("output" . 5) ("total" . 15)))
       (delta '(("type" . "text_delta") ("delta" . "chunk")))
       (m-upd (pi-event-message-update usage delta))
       (a-msg '(("role" . "assistant") ("content" . "done")))
       (m-end (pi-event-message-end a-msg))
       (args '(("command" . "ls")))
       (part '(("content" . "part1")))
       (te-start (pi-event-tool-execution-start "call-1" "bash" args))
       (te-upd (pi-event-tool-execution-update "call-1" "bash" args part))
       (te-end (pi-event-tool-execution-end "call-1" "bash" "ok" #f))
       (tool-res #())
       (t-end (pi-event-turn-end a-msg tool-res))
       (msgs (list u-msg a-msg))
       (a-end (pi-event-agent-end msgs #f))
       (a-settled (pi-event-agent-settled #f))
      ) ;

  (for-each (lambda (ev) (check-true (pi-event? ev)))
    (list sess a-start t-start m-start m-upd m-end te-start te-upd te-end t-end
      a-end a-settled
    ) ;list
  ) ;for-each
  (check-false (pi-event? '()))
  (check-false (pi-event? "not-an-event"))

  (for-each
    (lambda (test-case) (check (pi-event-type (car test-case)) => (cadr test-case)))
    (list (list sess "session")
      (list a-start "agent_start")
      (list t-start "turn_start")
      (list m-start "message_start")
      (list m-upd "message_update")
      (list m-end "message_end")
      (list te-start "tool_execution_start")
      (list te-upd "tool_execution_update")
      (list te-end "tool_execution_end")
      (list t-end "turn_end")
      (list a-end "agent_end")
      (list a-settled "agent_settled")
    ) ;list
  ) ;for-each

  (check (pi-event-session-version sess) => 3)
  (check (pi-event-session-id sess) => sid)
  (check (pi-event-session-timestamp sess) => ts)
  (check (pi-event-session-cwd sess) => cwd)

  (check (pi-event-message-start-message m-start) => u-msg)
  (check (pi-event-message-update-usage m-upd) => usage)
  (check (pi-event-message-update-assistant-message-event m-upd) => delta)
  (check (pi-event-message-end-message m-end) => a-msg)

  (check (pi-event-tool-execution-start-tool-call-id te-start) => "call-1")
  (check (pi-event-tool-execution-start-tool-name te-start) => "bash")
  (check (pi-event-tool-execution-start-args te-start) => args)

  (check (pi-event-tool-execution-update-partial-result te-upd) => part)

  (check (pi-event-tool-execution-end-result te-end) => "ok")
  (check (pi-event-tool-execution-end-is-error te-end) => #f)

  (check (pi-event-turn-end-message t-end) => a-msg)
  (check (pi-event-turn-end-tool-results t-end) => tool-res)

  (check (pi-event-agent-end-messages a-end) => msgs)
  (check (pi-event-agent-end-will-retry a-end) => #f)

  (check (pi-event-agent-settled-aborted a-settled) => #f)
) ;let*

;; 断言点 2: assoc 兼容访问不回归
(let* ((cwd "/tmp/test-assoc")
       (sid "custom-id-002")
       (ts "2026-10-19T00:00:00.000Z")
       (sess (pi-event-session cwd sid ts))
       (a-start (pi-event-agent-start))
       (t-start (pi-event-turn-start))
       (u-msg '(("role" . "user") ("content" . "hello")))
       (m-start (pi-event-message-start u-msg))
       (usage '(("input" . 1) ("output" . 2)))
       (delta '(("type" . "text_delta")))
       (m-upd (pi-event-message-update usage delta))
       (a-msg '(("role" . "assistant") ("content" . "world")))
       (m-end (pi-event-message-end a-msg))
       (te-start (pi-event-tool-execution-start "c2" "read" '(("path" . "a"))))
       (te-upd (pi-event-tool-execution-update "c2" "read" '(("path" . "a")) "pt"))
       (te-end (pi-event-tool-execution-end "c2" "read" "res" #t))
       (t-end (pi-event-turn-end a-msg #()))
       (a-end (pi-event-agent-end (list u-msg) #t))
       (a-settled (pi-event-agent-settled #t))
      ) ;

  (check (cdr (assoc "type" sess)) => "session")
  (check (cdr (assoc "version" sess)) => 3)
  (check (cdr (assoc "id" sess)) => sid)
  (check (cdr (assoc "timestamp" sess)) => ts)
  (check (cdr (assoc "cwd" sess)) => cwd)

  (check (cdr (assoc "type" a-start)) => "agent_start")
  (check (cdr (assoc "type" t-start)) => "turn_start")

  (check (cdr (assoc "type" m-start)) => "message_start")
  (check (cdr (assoc "message" m-start)) => u-msg)

  (check (cdr (assoc "type" m-upd)) => "message_update")
  (check (cdr (assoc "usage" m-upd)) => usage)
  (check (cdr (assoc "assistantMessageEvent" m-upd)) => delta)

  (check (cdr (assoc "type" m-end)) => "message_end")
  (check (cdr (assoc "message" m-end)) => a-msg)

  (check (cdr (assoc "type" te-start)) => "tool_execution_start")
  (check (cdr (assoc "toolCallId" te-start)) => "c2")
  (check (cdr (assoc "toolName" te-start)) => "read")

  (check (cdr (assoc "type" te-upd)) => "tool_execution_update")
  (check (cdr (assoc "toolCallId" te-upd)) => "c2")
  (check (cdr (assoc "partialResult" te-upd)) => "pt")

  (check (cdr (assoc "type" te-end)) => "tool_execution_end")
  (check (cdr (assoc "result" te-end)) => "res")
  (check (cdr (assoc "isError" te-end)) => #t)

  (check (cdr (assoc "type" t-end)) => "turn_end")
  (check (cdr (assoc "message" t-end)) => a-msg)

  (check (cdr (assoc "type" a-end)) => "agent_end")
  (check (cdr (assoc "willRetry" a-end)) => #t)

  (check (cdr (assoc "type" a-settled)) => "agent_settled")
  (check (cdr (assoc "aborted" a-settled)) => #t)
) ;let*

;; 断言点 3: pi-serialize-event JSON 输出与改造前一致（对比断言）
(let* ((cwd "/tmp/test-serialize")
       (sid "sid-12345")
       (ts "2026-10-19T01:00:00.000Z")
       (u-msg '(("role" . "user") ("content" . "hello")))
       (usage '(("input" . 10) ("output" . 20)))
       (delta '(("type" . "text_delta") ("delta" . "hi")))
       (a-msg '(("role" . "assistant") ("content" . "hi")))
       (args '(("command" . "ls")))
       (part '(("content" . "line1")))
       (pairs
         (list
           (cons (pi-event-session cwd sid ts)
             `(("type" . "session")
               ("version" . 3)
               ("id" . ,sid)
               ("timestamp" . ,ts)
               ("cwd" . ,cwd))
           ) ;cons
           (cons (pi-event-agent-start) '(("type" . "agent_start")))
           (cons (pi-event-turn-start) '(("type" . "turn_start")))
           (cons (pi-event-message-start u-msg)
             `(("type" . "message_start") ("message" . ,u-msg))
           ) ;cons
           (cons (pi-event-message-update usage delta)
             `(("type" . "message_update")
               ("usage" . ,usage)
               ("assistantMessageEvent" . ,delta))
           ) ;cons
           (cons (pi-event-message-end a-msg)
             `(("type" . "message_end") ("message" . ,a-msg))
           ) ;cons
           (cons (pi-event-tool-execution-start "call-99" "bash" args)
             `(("type" . "tool_execution_start")
               ("toolCallId" . "call-99")
               ("toolName" . "bash")
               ("args" . ,args))
           ) ;cons
           (cons (pi-event-tool-execution-update "call-99" "bash" args part)
             `(("type" . "tool_execution_update")
               ("toolCallId" . "call-99")
               ("toolName" . "bash")
               ("args" . ,args)
               ("partialResult" . ,part))
           ) ;cons
           (cons (pi-event-tool-execution-end "call-99" "bash" "ok" #f)
             '(("type" . "tool_execution_end")
               ("toolCallId" . "call-99")
               ("toolName" . "bash")
               ("result" . "ok")
               ("isError" . #f))
           ) ;cons
           (cons (pi-event-turn-end a-msg #())
             `(("type" . "turn_end") ("message" . ,a-msg) ("toolResults" . #()))
           ) ;cons
           (cons (pi-event-agent-end (list a-msg) #f)
             `(("type" . "agent_end")
               ("messages" . ,(list a-msg))
               ("willRetry" . #f))
           ) ;cons
           (cons (pi-event-agent-settled #f)
             '(("type" . "agent_settled") ("aborted" . #f))
           ) ;cons
         ) ;list
       ) ;pairs
      ) ;
  (for-each
    (lambda (p)
      (check (pi-serialize-event (car p)) => (pi-serialize-event (cdr p)))
    ) ;lambda
    pairs
  ) ;for-each
) ;let*

;; 断言点 4: bin/gf test tools/pi/tests/liii/pi/ 全过
(check (run "sh -c 'for f in tools/pi/tests/liii/pi/*-test.scm; do bin/gf test \"$f\" >/dev/null 2>&1 || exit 1; done'"
         :timeout 180
       ) ;run
  =>
  0
) ;check

;; 断言点 5: bin/gf pi --mode json 事件序列抽验与 bin/gf pi --help 可用
(check (run "bin/gf pi --help" :timeout 30) => 0)

(let* ((tmp-out "/tmp/pi-json-test-event-stream.jsonl")
       (ret (run "sh -c 'bin/gf pi --mode json --model fake-model --provider google ping > /tmp/pi-json-test-event-stream.jsonl 2>/dev/null'"
              :timeout 30
            ) ;run
       ) ;ret
       (lines
         (if (file-exists? tmp-out)
           (call-with-input-file tmp-out
             (lambda (p)
               (let loop
                 ((acc '()))
                 (let ((l (read-line p)))
                   (if (eof-object? l) (reverse acc) (loop (cons l acc)))
                 ) ;let
               ) ;let
             ) ;lambda
           ) ;call-with-input-file
           '()
         ) ;if
       ) ;lines
       (json-types
         (let loop
           ((ls lines) (acc '()))
           (if (null? ls)
             (reverse acc)
             (let* ((line (car ls))
                    (t
                      (guard (e (else #f))
                        (let ((j (string->json line)))
                          (and (pair? j)
                            (cond ((assoc "type" j) => cdr)
                                  ((assoc 'type j) => cdr)
                                  (else #f)
                            ) ;cond
                          ) ;and
                        ) ;let
                      ) ;guard
                    ) ;t
                   ) ;
               (if t (loop (cdr ls) (cons t acc)) (loop (cdr ls) acc))
             ) ;let*
           ) ;if
         ) ;let
       ) ;json-types
      ) ;
  (check-true (and (member "session" json-types string=?) #t))
  (check-true (and (member "agent_start" json-types string=?) #t))
  (check-true (and (member "turn_start" json-types string=?) #t))
  (check-true (and (member "turn_end" json-types string=?) #t))
  (check-true (and (member "agent_end" json-types string=?) #t))
  (check-true (and (member "agent_settled" json-types string=?) #t))
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

(check-report)
