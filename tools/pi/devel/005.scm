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

;; [005] record 化·消息结构 验收脚本
(set! *load-path* (cons "tools/pi" *load-path*))
(eval '(import (liii pi)) (rootlet))

;; 断言点 1: user/assistant/toolResult 消息与 text/toolCall block 为 define-record-type
(let* ((u-msg (pi-user-message "hello user" 1000))
       (txt-blk (pi-text-block "sample text"))
       (tc-blk (pi-tool-call-block "tc-001" "read" '(("path" . "foo.txt")) "sig-123"))
       (tc-blk2 (pi-tool-call-block "tc-002" "bash" '(("command" . "ls"))))
       (a-content (list txt-blk tc-blk))
       (usage-data '((input . 15) (output . 25) (total . 40)))
       (a-msg (pi-assistant-message a-content "stop" #f usage-data 2000))
       (a-msg-err (pi-assistant-message "fail" "error" "rate limited" #f 2500))
       (tr-content (list (pi-text-block "file contents")))
       (tr-msg (pi-tool-result-message "tc-001" "read" tr-content #f 3000))
       (tr-msg-err (pi-tool-result-message "tc-002" "bash" "not found" #t 3500))
      ) ;

  ;; 1.1 消息 record 谓词互斥与识别
  (for-each (lambda (m) (check-true (pi-message? m)) (check-false (pi-block? m)))
    (list u-msg a-msg tr-msg)
  ) ;for-each
  (check-true (pi-user-message? u-msg))
  (check-false (pi-assistant-message? u-msg))
  (check-false (pi-tool-result-message? u-msg))
  (check-true (pi-assistant-message? a-msg))
  (check-false (pi-user-message? a-msg))
  (check-false (pi-tool-result-message? a-msg))
  (check-true (pi-tool-result-message? tr-msg))
  (check-false (pi-user-message? tr-msg))
  (check-false (pi-assistant-message? tr-msg))

  ;; 1.2 block record 谓词互斥与识别
  (for-each (lambda (b) (check-true (pi-block? b)) (check-false (pi-message? b)))
    (list txt-blk tc-blk tc-blk2)
  ) ;for-each
  (check-true (pi-text-block? txt-blk))
  (check-false (pi-tool-call-block? txt-blk))
  (check-true (pi-tool-call-block? tc-blk))
  (check-false (pi-text-block? tc-blk))

  ;; 1.3 访问器与角色类型断言
  (check (pi-message-role u-msg) => "user")
  (check (pi-user-message-role u-msg) => "user")
  (check (pi-user-message-content u-msg) => "hello user")
  (check (pi-user-message-timestamp u-msg) => 1000)

  (check (pi-message-role a-msg) => "assistant")
  (check (pi-assistant-message-role a-msg) => "assistant")
  (check (pi-assistant-message-content a-msg) => a-content)
  (check (pi-assistant-message-stop-reason a-msg) => "stop")
  (check (pi-assistant-message-error-message a-msg) => #f)
  (check (pi-assistant-message-usage a-msg) => usage-data)
  (check (pi-assistant-message-timestamp a-msg) => 2000)
  (check (pi-assistant-message-stop-reason a-msg-err) => "error")
  (check (pi-assistant-message-error-message a-msg-err) => "rate limited")

  (check (pi-message-role tr-msg) => "toolResult")
  (check (pi-tool-result-message-role tr-msg) => "toolResult")
  (check (pi-tool-result-message-tool-call-id tr-msg) => "tc-001")
  (check (pi-tool-result-message-tool-name tr-msg) => "read")
  (check (pi-tool-result-message-content tr-msg) => tr-content)
  (check (pi-tool-result-message-is-error tr-msg) => #f)
  (check (pi-tool-result-message-timestamp tr-msg) => 3000)
  (check (pi-tool-result-message-is-error tr-msg-err) => #t)

  (check (pi-block-type txt-blk) => "text")
  (check (pi-text-block-type txt-blk) => "text")
  (check (pi-text-block-text txt-blk) => "sample text")

  (check (pi-block-type tc-blk) => "toolCall")
  (check (pi-tool-call-block-type tc-blk) => "toolCall")
  (check (pi-tool-call-block-id tc-blk) => "tc-001")
  (check (pi-tool-call-block-name tc-blk) => "read")
  (check (pi-tool-call-block-arguments tc-blk) => '(("path" . "foo.txt")))
  (check (pi-tool-call-block-thought-signature tc-blk) => "sig-123")
  (check (pi-tool-call-block-thought-signature tc-blk2) => #f)
) ;let*

;; 断言点 2: assoc 兼容访问不回归
(let* ((u-msg (pi-user-message "assoc user content" 1001))
       (txt-blk (pi-text-block "assoc text content"))
       (tc-blk (pi-tool-call-block "call-9" "grep" '(("pattern" . "foo")) "sig-xyz"))
       (a-msg (pi-assistant-message (list txt-blk tc-blk) "stop" #f '((total
                                                                        . 99)) 2002)
       ) ;a-msg
       (tr-msg (pi-tool-result-message "call-9" "grep" "match found" #f 3003))
       (cases
         (list (list "role" u-msg "user")
           (list "content" u-msg "assoc user content")
           (list "timestamp" u-msg 1001)
           (list "role" a-msg "assistant")
           (list "stopReason" a-msg "stop")
           (list "errorMessage" a-msg #f)
           (list "usage" a-msg '((total . 99)))
           (list "timestamp" a-msg 2002)
           (list "role" tr-msg "toolResult")
           (list "toolCallId" tr-msg "call-9")
           (list "toolName" tr-msg "grep")
           (list "content" tr-msg "match found")
           (list "isError" tr-msg #f)
           (list "timestamp" tr-msg 3003)
           (list "type" txt-blk "text")
           (list "text" txt-blk "assoc text content")
           (list "type" tc-blk "toolCall")
           (list "id" tc-blk "call-9")
           (list "name" tc-blk "grep")
           (list "arguments" tc-blk '(("pattern" . "foo")))
           (list "thoughtSignature" tc-blk "sig-xyz")
         ) ;list
       ) ;cases
      ) ;
  (for-each
    (lambda (c)
      (let ((str-key (car c)) (obj (cadr c)) (expected (caddr c)))
        (check (cdr (assoc str-key obj)) => expected)
        (check (cdr (assoc (string->symbol str-key) obj)) => expected)
      ) ;let
    ) ;lambda
    cases
  ) ;for-each
) ;let*

;; 断言点 3: JSON 序列化输出与改造前一致（对比断言）
(let* ((u-msg-rec (pi-user-message "hello" 1000))
       (u-msg-alist '(("role" . "user")
                      ("content" . "hello")
                      ("timestamp" . 1000)))
       (txt-rec (pi-text-block "txt"))
       (txt-alist '(("type" . "text") ("text" . "txt")))
       (tc-rec (pi-tool-call-block "tc-1" "read" '(("path" . "f")) "sig-1"))
       (tc-alist '(("type" . "toolCall")
                   ("id" . "tc-1")
                   ("name" . "read")
                   ("arguments" ("path" . "f"))
                   ("thoughtSignature" . "sig-1"))
       ) ;tc-alist
       (a-msg-rec (pi-assistant-message (list txt-rec tc-rec) "stop" #f '((input
                                                                            . 10)) 2000)
       ) ;a-msg-rec
       (a-msg-alist
         `(("role" . "assistant")
           ("content" ,txt-alist ,tc-alist)
           ("stopReason" . "stop")
           ("usage" (input . 10))
           ("timestamp" . 2000))
       ) ;a-msg-alist
       (tr-msg-rec (pi-tool-result-message "tc-1" "read" "file content" #f 3000))
       (tr-msg-alist '(("role" . "toolResult")
                       ("toolCallId" . "tc-1")
                       ("toolName" . "read")
                       ("content" . "file content")
                       ("isError" . #f)
                       ("timestamp" . 3000))
       ) ;tr-msg-alist
      ) ;
  (check (pi-block->alist txt-rec) => txt-alist)
  (check (pi-block->alist tc-rec) => tc-alist)
  (check (pi-message->alist u-msg-rec) => u-msg-alist)
  (check (pi-message->alist tr-msg-rec) => tr-msg-alist)
  (check (pi-serialize-event (pi-event-message-start u-msg-rec))
    =>
    (pi-serialize-event (pi-event-message-start u-msg-alist))
  ) ;check
  (check (pi-serialize-event (pi-event-message-end a-msg-rec))
    =>
    (pi-serialize-event (pi-event-message-end a-msg-alist))
  ) ;check
  (check (pi-serialize-event (pi-event-turn-end a-msg-rec (vector tr-msg-rec)))
    =>
    (pi-serialize-event (pi-event-turn-end a-msg-alist (vector tr-msg-alist)))
  ) ;check
  (check (pi-serialize-event (pi-event-agent-end (list u-msg-rec a-msg-rec) #f))
    =>
    (pi-serialize-event (pi-event-agent-end (list u-msg-alist a-msg-alist) #f))
  ) ;check
) ;let*

;; 断言点 4: bin/gf test tools/pi/tests/liii/ 全过
(check (run "sh -c 'for f in tools/pi/tests/liii/*-test.scm tools/pi/tests/liii/pi/*-test.scm; do bin/gf test \"$f\" >/dev/null 2>&1 || exit 1; done'"
         :timeout 300
       ) ;run
  =>
  0
) ;check

;; 断言点 5: bin/gf pi -p 与 --mode json 冒烟可用
(check (run "bin/gf pi --help >/dev/null 2>&1" :timeout 30) => 0)

;; 5.1 bin/gf pi -p 冒烟验证
(let* ((tmp-out "/tmp/pi-005-smoke-print.txt")
       (ret (run "sh -c 'bin/gf pi -p --no-session \"1+1 等于几？\" > /tmp/pi-005-smoke-print.txt 2>/dev/null'"
              :timeout 60
            ) ;run
       ) ;ret
       (out
         (if (file-exists? tmp-out)
           (call-with-input-file tmp-out
             (lambda (p)
               (let loop
                 ((acc '()))
                 (let ((l (read-line p)))
                   (if (eof-object? l) (string-join (reverse acc) "\n") (loop (cons l acc)))
                 ) ;let
               ) ;let
             ) ;lambda
           ) ;call-with-input-file
           ""
         ) ;if
       ) ;out
      ) ;
  (check ret => 0)
  (check-true (> (string-length (string-trim out)) 0))
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

;; 5.2 bin/gf pi --mode json 冒烟验证（校验 JSON 事件流完整性）
(let* ((tmp-out "/tmp/pi-005-smoke-json.jsonl")
       (ret (run "sh -c 'bin/gf pi --mode json --no-session --max-turns 1 \"1+1 等于几？\" > /tmp/pi-005-smoke-json.jsonl 2>/dev/null'"
              :timeout 60
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
  (check ret => 0)
  (for-each
    (lambda (type-name)
      (check-true (and (member type-name json-types string=?) #t))
    ) ;lambda
    (list "session" "agent_start" "turn_start" "message_start" "message_end"
      "turn_end" "agent_end" "agent_settled"
    ) ;list
  ) ;for-each
  (if (file-exists? tmp-out) (delete-file tmp-out))
) ;let*

(check-report)
