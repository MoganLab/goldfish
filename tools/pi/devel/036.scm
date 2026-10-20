(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  ;; 显式导入 (scheme process-context)：避免 get-environment-variable 解析到破损绑定（任务 038）
  (scheme process-context)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii uuid)
  (liii subprocess)
  (liii pi bash)
  (liii pi session)
  (liii pi args)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; 辅助定义（常量、命令捕获、pi 子进程、stub 会话 fixture）见同目录 036-support.scm
(load "tools/pi/devel/036-support.scm")

(display "=== 验收点 1: 形态对齐（--compact / --no-auto-compact；split-turn 不做） ===\n"
) ;display
(let ((help (%run-capture "help" "timeout 30 bin/gf pi --help" 60)))
  (check (%rc help) => 0)
  (check-true (string-contains? (%out help) "--compact"))
  (check-true (string-contains? (%out help) "--no-auto-compact"))
) ;let
(check (pi-args-ref (parse-pi-args '("--compact")) 'compact) => #t)
(check (pi-args-ref (parse-pi-args '("--no-auto-compact" "hi")) 'no-auto-compact)
  =>
  #t
) ;check
(check (cdr (assoc 'is-split-turn (pi-find-cut-point '() 20000))) => #f)
(%ok "test-args"
  "timeout 120 bin/gf test tools/pi/tests/liii/pi-args-test.scm"
  150
) ;%ok

(display "=== 验收点 2/3: 压缩消息构造、截断策略、默认设置 ===\n"
) ;display
(check PI_COMPACTION_SUMMARY_PREFIX
  =>
  "The conversation history before this point was compacted into the following summary:\n\n<summary>\n"
) ;check
(check PI_COMPACTION_SUMMARY_SUFFIX => "\n</summary>")
(let* ((text (pi-user-message-content (pi-compaction-summary-message "S-036"))))
  (check text
    =>
    (string-append PI_COMPACTION_SUMMARY_PREFIX
      "S-036"
      PI_COMPACTION_SUMMARY_SUFFIX
    ) ;string-append
  ) ;check
) ;let*
(let ((d (pi-compaction-default-settings)))
  (check (cdr (assoc 'enabled d)) => #t)
  (check (cdr (assoc 'reserve-tokens d)) => 16384)
  (check (cdr (assoc 'keep-recent-tokens d)) => 20000)
) ;let
(let ((s (pi-load-compaction-settings "tools/pi/tests/fixtures/compact-settings")))
  (check (cdr (assoc 'enabled s)) => #f)
  (check (cdr (assoc 'reserve-tokens s)) => 8000)
  (check (cdr (assoc 'keep-recent-tokens s)) => 9000)
) ;let
(let* ((tool
         (list (cons "type" "message")
           (cons "id" "t1")
           (cons "message" (list (cons "role" "toolResult") (cons "content" "x")))
         ) ;list
       ) ;tool
       (user
         (list (cons "type" "message")
           (cons "id" "u1")
           (cons "message" (list (cons "role" "user") (cons "content" "hi")))
         ) ;list
       ) ;user
      ) ;
  (check (pi-valid-cut-point? tool) => #f)
  (check (pi-valid-cut-point? user) => #t)
) ;let*
(let ((fixture "tools/pi/tests/fixtures/compact/sample-session.jsonl"))
  (check
    (cdr (assoc 'first-kept-index
           (pi-find-cut-point (pi-read-session-entries fixture) 20000)
         ) ;assoc
    ) ;cdr
    =>
    1
  ) ;check
) ;let
(%ok "test-compact"
  "timeout 150 bin/gf test tools/pi/tests/liii/pi/compact/ tools/pi/tests/liii/pi/pi-load-compaction-settings-test.scm tools/pi/tests/liii/pi/pi-compact-e2e-test.scm"
  180
) ;%ok

(%run-shell (string-append "rm -rf " ACC) 30)

(define AGENT-MAIN (%agent "main" DEFAULT-SETTINGS))

(define AGENT-FLAG (%agent "flag" HUGE-ON))

(define AGENT-OFF (%agent "off" HUGE-OFF))

(define AGENT-EMPTY (%agent "empty" DEFAULT-SETTINGS))

(define SFILE-MAIN (%session AGENT-MAIN))

(define SFILE-FLAG (%session AGENT-FLAG))

(define SFILE-OFF (%session AGENT-OFF))

(let* ((stub (process-spawn (list "python3" STUB)
               :stdin  'null
               :stdout 'pipe
               :stderr 'discard
             ) ;process-spawn
       ) ;stub
       (line (process-read-line stub))
       (port
         (if (and (string? line) (string-starts? line "PORT:"))
           (string->number (string-trim-both (substring line 5 (string-length line))))
           #f
         ) ;if
       ) ;port
       (base (string-append "http://127.0.0.1:" (number->string (or port 1))))
       (env-main (%env AGENT-MAIN base))
      ) ;
  (check-true (number? port))
  (dynamic-wind (lambda () #t)
    (lambda ()
      (display "=== 验收点 4/5: --compact 手动压缩、原文保留、压缩后继续 ===\n"
      ) ;display
      (let ((r (%spawn-pi (list "--compact") env-main 60000)))
        (check (%code r) => 0)
        (check-true (string-contains? (%stdout r) "Compacted session"))
        (check-true (string-contains? (%stderr r) "Compacted session"))
        ;; 验收点 9：stderr 不含 key 与会话正文
        (check-false (string-contains? (%stderr r) "test-google-key-from-auth-json"))
        (check-false (string-contains? (%stderr r) "next question"))
        (let* ((entries (pi-read-session-entries SFILE-MAIN)) (last (car (reverse entries))))
          (check (%ncomp SFILE-MAIN) => 1)
          (check (%ref last "type") => "compaction")
          (for-each
            (lambda (k) (check-true (string? (%ref last k))))
            '("id" "parentId" "timestamp" "summary" "firstKeptEntryId")
          ) ;for-each
          (check-true (> (string-length (%ref last "summary")) 0))
          (check-true (> (%ref last "tokensBefore") 0))
          (check (%ref (%ref last "details") "reason") => "manual")
          ;; 原文一条不少（4 条 message 仍在），firstKeptEntryId 指向现存条目
          (check (%nmsg SFILE-MAIN) => 4)
          (check-true
            (pair?
              (member (%ref last "firstKeptEntryId") (map (lambda (e) (%ref e "id")) entries))
            ) ;pair?
          ) ;check-true
          ;; 投影首条为 summary 包装的 user 消息
          (let* ((first (car (pi-project-session-messages entries)))
                 (text (pi-user-message-content first))
                ) ;
            (check (pi-message-role first) => "user")
            (check-true (string-contains? text
                          "The conversation history before this point was compacted"
                        ) ;string-contains?
            ) ;check-true
            (check-true (string-contains? text (%ref last "summary")))
          ) ;let*
        ) ;let*
      ) ;let
      (let ((r (%spawn-pi (list "--compact" "follow up") env-main 60000)))
        (check (%code r) => 0)
        (check-true (string-contains? (%stdout r) "Hello stream!"))
        (check-true (> (%nmsg SFILE-MAIN) 4))
        (check-true (>= (%ncomp SFILE-MAIN) 2))
      ) ;let
      (let ((before (%ncomp SFILE-MAIN))
            (r (%spawn-pi (list "-c" "-p" "continue after compact") env-main 60000))
           ) ;
        (check (%code r) => 0)
        (check-true (string-contains? (%stdout r) "Hello stream!"))
        (check (%ncomp SFILE-MAIN) => before)
      ) ;let

      (display "=== 验收点 6: 阈值自动压缩与关闭开关 ===\n")
      (path-write-text (path-join (path AGENT-MAIN) (path "settings.json")) HUGE-ON)
      (let ((before (%ncomp SFILE-MAIN))
            (r (%spawn-pi (list "-c" "-p" "auto compact please") env-main 60000))
           ) ;
        (check (%code r) => 0)
        (check-true (string-contains? (%stderr r) "Auto-compacted session"))
        (check-true (> (%ncomp SFILE-MAIN) before))
      ) ;let
      (let ((r (%spawn-pi (list "-c" "--no-auto-compact" "-p" "no auto flag")
                 (%env AGENT-FLAG base)
                 60000
               ) ;%spawn-pi
            ) ;r
           ) ;
        (check (%code r) => 0)
        (check-false (string-contains? (%stderr r) "Auto-compacted"))
        (check (%ncomp SFILE-FLAG) => 0)
      ) ;let
      (let ((r (%spawn-pi (list "-c" "-p" "no auto here") (%env AGENT-OFF base) 60000)))
        (check (%code r) => 0)
        (check-false (string-contains? (%stderr r) "Auto-compacted"))
        (check (%ncomp SFILE-OFF) => 0)
      ) ;let

      (display "=== 验收点 7/8: 错误路径与超时，不写坏会话 ===\n")
      (let ((r (%spawn-pi (list "--compact") (%env AGENT-EMPTY base) 60000)))
        (check (%code r) => 1)
        (check-true (string-contains? (%stderr r) "No session found to compact"))
      ) ;let
      (let ((before (%ncomp SFILE-MAIN))
            (r (%spawn-pi (list "--compact")
                 (%env AGENT-MAIN (string-append base "/error-500"))
                 90000
               ) ;%spawn-pi
            ) ;r
           ) ;
        (check (%code r) => 1)
        (check-true (string-contains? (%stderr r) "Summarization failed"))
        (check (%ncomp SFILE-MAIN) => before)
      ) ;let
      (let ((before (%ncomp SFILE-MAIN))
            (r (%spawn-pi (list "--compact" "--timeout" "2")
                 (%env AGENT-MAIN (string-append base "/stream-idle-hang"))
                 90000
               ) ;%spawn-pi
            ) ;r
           ) ;
        (check (%code r) => 1)
        (check-true (string-contains? (%stderr r) "timed out"))
        (check (%ncomp SFILE-MAIN) => before)
      ) ;let
    ) ;lambda
    (lambda () (process-kill stub) (process-close stub))
  ) ;dynamic-wind
) ;let*

(display "=== 验收点 10: 门禁（build / fmt / diff / check / doc / 行数） ===\n"
) ;display
(%ok "build" "timeout 280 xmake b goldfish" 290)
(%ok "fmt" "timeout 120 bin/gf fmt --check" 150)
(%ok "check" "timeout 200 bin/gf check" 220)
(%ok "doc" "timeout 120 bin/gf -I tools/pi doc --build-json" 150)
(let ((q (%run-capture "doc-query"
           "timeout 60 bin/gf -I tools/pi doc pi-find-cut-point"
           90
         ) ;%run-capture
      ) ;q
     ) ;
  (check (%rc q) => 0)
  (check-true (string-contains? (%out q) "pi-find-cut-point"))
) ;let
(for-each
  (lambda (f) (check (< (length (path-read-lines f)) 300) => #t))
  '("tools/pi/liii/pi/compact.scm" "tools/pi/liii/pi/compact/tokens.scm"
    "tools/pi/liii/pi/compact/cut.scm" "tools/pi/liii/pi/compact/serialize.scm"
    "tools/pi/liii/pi/compact/fileops.scm" "tools/pi/liii/pi/compact/prompt.scm"
    "tools/pi/liii/pi/compact/summary.scm"
    "tools/pi/liii/pi/compact/session.scm" "tools/pi/liii/pi/compact/flow.scm"
    "tools/pi/liii/pi/agent.scm" "tools/pi/liii/pi/modes.scm")
) ;for-each

(display "=== 验收点 11: 既有行为零回归（tools/pi/tests/liii 全量） ===\n"
) ;display
(let ((r (%run-capture "regression" "timeout 280 bin/gf test tools/pi/tests/liii/" 290)
      ) ;r
     ) ;
  (check (%rc r) => 0)
  (check-false (string-contains? (%out r) "Failed:"))
) ;let

(check-report)
