(define %root (path->string (path-cwd)))

(define %fixture (string-append %root "/tools/pi/tests/fixtures/direct"))

(define %stub (string-append %root "/tools/pi/tests/stub_blueprint_server.py"))

(define %tmp-root (path->string (path-join (path-temp-dir) "gf-047-accept")))

(define %tmp1 (string-append %tmp-root "/work1"))

(define %tmp2 (string-append %tmp-root "/work2"))

(define %tmp-files '())

(define %stub-proc #f)

(define (%sh tag cmd timeout)
  (%setup-dir! %tmp-root)
  (let* ((out (string-append %tmp-root "/" tag ".out"))
         (err (string-append %tmp-root "/" tag ".err"))
        ) ;
    (set! %tmp-files (append (list out err) %tmp-files))
    (let* ((rc (run (list (pi-find-bash-path) "-c" (string-append "(" cmd ") >" out " 2>" err))
                 :stdin   'null
                 :timeout timeout
               ) ;run
           ) ;rc
           (o (if (file-exists? out) (path-read-text out) ""))
           (e (if (file-exists? err) (path-read-text err) ""))
          ) ;
      (list rc o e)
    ) ;let*
  ) ;let*
) ;define

(define (%ok? tag cmd timeout)
  (car (%sh tag cmd timeout))
) ;define

(define (%cleanup!)
  (when %stub-proc
    (process-kill %stub-proc)
    (process-close %stub-proc)
    (set! %stub-proc #f)
  ) ;when
  (for-each (lambda (f) (when (file-exists? f) (delete-file f))) %tmp-files)
  (when (file-exists? %tmp-root)
    (run (list (pi-find-bash-path) "-c" (string-append "rm -rf " %tmp-root))
      :stdin   'null
      :timeout 60
    ) ;run
  ) ;when
  (set! %tmp-files '())
) ;define

(define (%setup-dir! dir)
  (unless (path-exists? (path dir))
    (path-mkdir (path dir) :parents #t :exist-ok #t)
  ) ;unless
) ;define

(define (%env base)
  (list (cons "PI_CODING_AGENT_DIR" %fixture)
    (cons "GF_PI_BASE_URL" base)
    (cons "GF_PI_MODEL" "gemini-3.8-flash")
    (cons "PATH" (or (get-environment-variable "PATH") "/usr/bin:/bin"))
    (cons "HOME" (or (get-environment-variable "HOME") "/tmp"))
    (cons "https_proxy" "")
    (cons "http_proxy" "")
    (cons "all_proxy" "")
    (cons "HTTPS_PROXY" "")
    (cons "HTTP_PROXY" "")
    (cons "ALL_PROXY" "")
  ) ;list
) ;define

(define (%run-pi dir base args)
  (let ((p (process-spawn (cons (string-append %root "/bin/gf") (cons "pi" args))
             :cwd    dir
             :env    (%env base)
             :stdout 'pipe
             :stderr 'pipe
           ) ;process-spawn
        ) ;p
       ) ;
    (let* ((code (process-wait p 240000))
           (out (process-read p :stream 'stdout))
           (err (process-read p :stream 'stderr))
          ) ;
      (process-close p)
      (list code (if (string? out) out "") (if (string? err) err ""))
    ) ;let*
  ) ;let
) ;define

(define (%start-stub!)
  (let* ((p (process-spawn (list "python3" %stub) :stdout 'pipe))
         (line (process-read-line p))
        ) ;
    (set! %stub-proc p)
    (check-true (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
  ) ;let*
) ;define

(define (%state-file dir)
  (string-append dir "/.pi/blueprint.json")
) ;define

(define (%load-bp dir)
  (pi-blueprint-load-state (%state-file dir))
) ;define

(define (%phase-status dir phase-id)
  (pi-phase-status (pi-blueprint-find-phase (%load-bp dir) phase-id))
) ;define

(define (%set-first-gate-command! dir command)
  (let* ((bp (%load-bp dir))
         (bp1
           (pi-blueprint-update-phase bp
             "1"
             (lambda (phase)
               (let loop
                 ((gates (pi-phase-gates phase)) (i 0) (acc '()))
                 (if (null? gates)
                   (pi-blueprint-set-field phase "verification_gates" (list->vector (reverse acc)))
                   (loop (cdr gates)
                     (+ i 1)
                     (cons (if (= i 0) (pi-blueprint-set-field (car gates) "command" command) (car gates))
                       acc
                     ) ;cons
                   ) ;loop
                 ) ;if
               ) ;let
             ) ;lambda
           ) ;pi-blueprint-update-phase
         ) ;bp1
        ) ;
    (pi-blueprint-save-state! (%state-file dir) bp1)
  ) ;let*
) ;define

(define (%check-jsonl out)
  (let ((lines (filter (lambda (s) (not (string-null? s))) (string-split out "\n"))))
    (check-true (>= (length lines) 4))
    (if (< (length lines) 4)
      (check-true #f)
      (begin
        (for-each
          (lambda (line) (check-true (pair? (string->json line))))
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
      ) ;begin
    ) ;if
  ) ;let
) ;define

(define (%check-arg-parsing!)
  (display "=== 验收点 1: CLI 参数解析与互斥 ===\n")
  (check (pi-args-ref (parse-pi-args '("--blueprint" "目标")) 'blueprint) => #t)
  (check (pi-args-ref (parse-pi-args '("--plan-status")) 'plan-status) => #t)
  (check (pi-args-ref (parse-pi-args '("--plan-next")) 'plan-next) => #t)
  (check (pi-args-ref (parse-pi-args '("--plan-verify")) 'plan-verify) => #t)
  (check (pi-args-ref (parse-pi-args '("--plan-abandon")) 'plan-abandon) => #t)
  (check (pi-args-ref (parse-pi-args '("--plan-verify" "--approve")) 'approve)
    =>
    #t
  ) ;check
  (check (pi-args-ref (parse-pi-args '("--blueprint-file=x.json")) 'blueprint-file)
    =>
    "x.json"
  ) ;check
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--blueprint"
                                               "x"
                                               "--plan-status")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--blueprint"
                                               "x"
                                               "--code-review")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--approve")))
  ) ;check-catch

) ;define
