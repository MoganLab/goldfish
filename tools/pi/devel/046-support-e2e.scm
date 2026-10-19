;; [046] 验收脚本的 stub server 端到端检查（由 tools/pi/devel/046.scm 通过
;; (load ...) 调用）。覆盖验收点 5（print / JSON 阶段摘要分流）与验收点 7
;; （零外部状态：fixture 配置目录快照不变）。

(define %fixture "tools/pi/tests/fixtures/direct")

(define (%as-text x)
  (if (string? x) x "")
) ;define

(define (%snapshot)
  (cadr (%sh "snapshot" (string-append "find " %fixture " -type f | sort") 30))
) ;define

(define (%e2e-env base-url)
  (list (cons "PI_CODING_AGENT_DIR" (string-append %root "/" %fixture))
    (cons "GF_PI_BASE_URL" base-url)
    (cons "GF_PI_MODEL" "gemini-3.8-flash")
    (cons "https_proxy" "")
    (cons "http_proxy" "")
    (cons "all_proxy" "")
    (cons "HTTPS_PROXY" "")
    (cons "HTTP_PROXY" "")
    (cons "ALL_PROXY" "")
  ) ;list
) ;define

(define (%start-stub!)
  (let* ((p (process-spawn (list "python3" "tools/pi/tests/stub_gemini_server.py")
              :stdout 'pipe
            ) ;process-spawn
         ) ;p
         (line (process-read-line p))
        ) ;
    (check-true (string-starts? line "PORT:"))
    (cons p
      (string->number (string-trim-both (substring line 5 (string-length line))))
    ) ;cons
  ) ;let*
) ;define

(define (%run-pi args env)
  (let ((p (process-spawn (cons (string-append %root "/bin/gf") (cons "pi" args))
             :env    env
             :stdout 'pipe
             :stderr 'pipe
           ) ;process-spawn
        ) ;p
       ) ;
    (let* ((code (process-wait p 240000))
           (out (%as-text (process-read p :stream 'stdout)))
           (err (%as-text (process-read p :stream 'stderr)))
          ) ;
      (process-close p)
      (list code out err)
    ) ;let*
  ) ;let
) ;define

(define (%check-print-mode env)
  (let* ((res (%run-pi '("--print" "--red-green" "--no-session" "--timeout" "30"
                         "--session-timeout" "120" "写一个加法测试")
                env
              ) ;%run-pi
         ) ;res
         (code (car res))
         (out (cadr res))
         (err (caddr res))
        ) ;
    (check code => 0)
    (check-true (string-contains? out "## TDD Summary"))
    (check-true (string-contains? out "Phase: red"))
    (check-true (string-contains? out "Task: 写一个加法测试"))
    (check-true (or (string-contains? out "Hello stream!")
                  (string-contains? out "Hello from stub!")
                ) ;or
    ) ;check-true
    (check-false (string-contains? err "## TDD Summary"))
    (check-false (string-contains? err "Authorization"))
    (check-false (string-contains? err "api-key"))
  ) ;let*
) ;define

(define (%check-json-mode env)
  (let* ((res (%run-pi '("--mode" "json" "--red-green" "--no-session"
                         "--timeout" "30" "--session-timeout" "120"
                         "写一个加法测试")
                env
              ) ;%run-pi
         ) ;res
         (code (car res))
         (out (cadr res))
         (err (caddr res))
         (lines
           (filter (lambda (s) (not (string-null? s))) (string-split out "\n"))
         ) ;lines
        ) ;
    (check code => 0)
    (check-true (>= (length lines) 4))
    (for-each
      (lambda (line) (check-true (list? (string->json line))))
      lines
    ) ;for-each
    (check
      (cdr (assoc "type" (string->json (car lines))))
      =>
      "session"
    ) ;check
    (check
      (cdr
        (assoc "type" (string->json (car (reverse lines))))
      ) ;cdr
      =>
      "agent_settled"
    ) ;check
    (check-true (string-contains? err "## TDD Summary"))
    (check-true (string-contains? err "Phase: red"))
    (check-false (string-contains? out "## TDD Summary"))
  ) ;let*
) ;define

(define (run-e2e-checks!)
  (display "=== 验收点 5/7: stub server 端到端（print / JSON 摘要与零外部状态） ===\n"
  ) ;display
  (let* ((stub (%start-stub!))
         (base (string-append "http://127.0.0.1:" (number->string (cdr stub))))
         (env (%e2e-env base))
         (before (%snapshot))
        ) ;
    (dynamic-wind (lambda () #f)
      (lambda ()
        (%check-print-mode env)
        (%check-json-mode env)
        (check (%snapshot) => before)
      ) ;lambda
      (lambda () (process-kill (car stub)) (process-close (car stub)))
    ) ;dynamic-wind
  ) ;let*
) ;define
