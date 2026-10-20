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

;; [015] record 化·收尾全量冒烟 验收脚本
(set! *load-path* (cons "tools/pi" *load-path*))

;; ============================================================================
;; 验收点 1: bin/gf pi -p 基本对话退出码 0，stdout 输出非空
;; ============================================================================
(let* ((tmp-out "/tmp/pi-015-smoke-print.txt")
       (ret (run "sh -c 'bin/gf pi -p --no-session \"reply exactly pong\" > /tmp/pi-015-smoke-print.txt 2>/dev/null'"
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

;; ============================================================================
;; 验收点 2: bin/gf pi --mode json 事件序列含 agent_settled，退出码 0
;; ============================================================================
(let* ((tmp-out "/tmp/pi-015-smoke-json.jsonl")
       (ret (run "sh -c 'bin/gf pi --mode json --no-session --max-turns 1 \"reply exactly pong\" > /tmp/pi-015-smoke-json.jsonl 2>/dev/null'"
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
  (check-true (and (member "agent_settled" json-types string=?) #t))
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

;; ============================================================================
;; 验收点 3: bin/gf pi -p @/nonexistent 退出码 1 且 stderr 人可读
;; ============================================================================
(let* ((tmp-err "/tmp/pi-015-smoke-err.txt")
       (ret (run "sh -c 'bin/gf pi -p @/nonexistent >/dev/null 2> /tmp/pi-015-smoke-err.txt'"
              :timeout 30
            ) ;run
       ) ;ret
       (err-out
         (if (file-exists? tmp-err)
           (call-with-input-file tmp-err
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
       ) ;err-out
      ) ;
  (check ret => 1)
  (check-true (> (string-length (string-trim err-out)) 0))
  (check-true (string-contains err-out "cannot read"))
  (check-true (string-contains err-out "/nonexistent"))
  (if (file-exists? tmp-err) (delete-file tmp-err))
) ;let*

;; ============================================================================
;; 验收点 4: bin/gf test 抽样全过（覆盖 004-007 record 结构、消息/事件/配置/工具）
;; ============================================================================
(let ((sample-tests (list
                      ;; 消息结构 (004)
                      "tools/pi/tests/liii/pi/pi-user-message-test.scm"
                      "tools/pi/tests/liii/pi/pi-assistant-message-test.scm"
                      "tools/pi/tests/liii/pi/pi-tool-result-message-test.scm"
                      ;; 事件结构 (004)
                      "tools/pi/tests/liii/pi/pi-event-test.scm"
                      "tools/pi/tests/liii/pi/pi-event-agent-settled-test.scm"
                      "tools/pi/tests/liii/pi/pi-serialize-event-test.scm"
                      ;; 配置结构 (006)
                      "tools/pi/tests/liii/pi/pi-settings-record-test.scm"
                      "tools/pi/tests/liii/pi/pi-auth-record-test.scm"
                      "tools/pi/tests/liii/pi/pi-model-metadata-record-test.scm"
                      ;; 工具结构 (007)
                      "tools/pi/tests/liii/pi/pi-tool-definition-record-test.scm"
                      "tools/pi/tests/liii/pi/pi-tool-result-record-test.scm"
                      "tools/pi/tests/liii/pi/pi-trunc-result-record-test.scm"
                      ;; 工具执行与请求体
                      "tools/pi/tests/liii/pi/pi-execute-bash-test.scm"
                      "tools/pi/tests/liii/pi/pi-execute-read-test.scm"
                      "tools/pi/tests/liii/pi/gemini-build-request-body-test.scm"
                      "tools/pi/tests/liii/pi/anthropic-build-request-body-test.scm"
                    ) ;list
      ) ;sample-tests
     ) ;
  (for-each
    (lambda (t)
      (let ((cmd (string-append "bin/gf test " t)))
        (check (run cmd :timeout 60) => 0)
      ) ;let
    ) ;lambda
    sample-tests
  ) ;for-each
) ;let

;; ============================================================================
;; 验收点 5: tools/pi 全量单元测试全过
;; ============================================================================
(check (run "sh -c 'for f in tools/pi/tests/liii/*-test.scm tools/pi/tests/liii/pi/*-test.scm; do bin/gf test \"$f\" >/dev/null 2>&1 || exit 1; done'"
         :timeout 300
       ) ;run
  =>
  0
) ;check

;; ============================================================================
;; 验收点 6: bin/gf check 门禁检查与 CLI 帮助可用
;; ============================================================================
(check (run "bin/gf check" :timeout 120) => 0)
(check (run "bin/gf pi --help >/dev/null 2>&1" :timeout 30) => 0)

(check-report)
