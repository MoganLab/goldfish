;; [047] blueprint 命令行端到端测试的辅助函数（由 pi-blueprint-cli-e2e-test.scm
;; 通过 (load ...) 调用）。不引用计划文档，全部测试数据来自 fixtures 与自建 stub。

(define (find-repo-root dir)
  (if (path-exists? (path-join dir "gfproject.json"))
    (path->string dir)
    (let ((parent (path-parent dir)))
      (if (path=? parent dir) (path->string dir) (find-repo-root parent))
    ) ;let
  ) ;if
) ;define

(define %root (find-repo-root (path-cwd)))

(define %fixture
  (path->string (path-join (path %root) "tools/pi/tests/fixtures/direct"))
) ;define

(define %gf (executable))

(define %stub
  (path->string (path-join (path %root) "tools/pi/tests/stub_blueprint_server.py")
  ) ;path->string
) ;define

(define %tmp1 (path->string (path-join (path-temp-dir) "gf-bp-e2e-1")))

(define %tmp2 (path->string (path-join (path-temp-dir) "gf-bp-e2e-2")))

(define %tmp-agent (path->string (path-join (path-temp-dir) "gf-bp-e2e-agent")))

(define (as-text x)
  (if (string? x) x "")
) ;define

(define (setup-dir! dir)
  (unless (path-exists? (path dir))
    (path-mkdir (path dir) :parents #t :exist-ok #t)
  ) ;unless
) ;define

(define (setup-fixture!)
  (setup-dir! %tmp-agent)
  (for-each
    (lambda (name)
      (path-copy (path-join (path %fixture) (path name))
        (path-join (path %tmp-agent) (path name))
      ) ;path-copy
    ) ;lambda
    '("auth.json" "models-store.json" "settings.json")
  ) ;for-each
) ;define

(define (cleanup-fixture!)
  (when (path-exists? (path %tmp-agent))
    (g_rmtree %tmp-agent)
  ) ;when
) ;define

(define (cleanup-dir! dir)
  (let ((pi-dir (path->string (path-join (path dir) ".pi"))))
    (when (path-exists? (path pi-dir))
      (for-each
        (lambda (name)
          (let ((f (path->string (path-join (path pi-dir) name))))
            (when (path-exists? (path f))
              (delete-file f)
            ) ;when
          ) ;let
        ) ;lambda
        '("blueprint.json" "blueprint.md" "blueprint.history.jsonl")
      ) ;for-each
      (path-rmdir (path pi-dir))
    ) ;when
    (when (path-exists? (path dir))
      (path-rmdir (path dir))
    ) ;when
  ) ;let
) ;define

(define (start-stub!)
  (let* ((p (process-spawn (list "python3" %stub) :stdout 'pipe))
         (line (process-read-line p))
        ) ;
    (check-true (string-starts? line "PORT:"))
    (cons p
      (string->number (string-trim-both (substring line 5 (string-length line))))
    ) ;cons
  ) ;let*
) ;define

(define (e2e-env base)
  (append (list (cons "PI_CODING_AGENT_DIR" %tmp-agent)
            (cons "GF_PI_BASE_URL" base)
            (cons "GF_PI_MODEL" "gemini-3.8-flash")
            (cons "https_proxy" "")
            (cons "http_proxy" "")
            (cons "all_proxy" "")
            (cons "HTTPS_PROXY" "")
            (cons "HTTP_PROXY" "")
            (cons "ALL_PROXY" "")
          ) ;list
    (get-environment-variables)
  ) ;append
) ;define

(define (read-all p . maybe-stream)
  (let ((stream (if (null? maybe-stream) 'stdout (car maybe-stream))))
    (let loop
      ((acc ""))
      (let ((c (process-read p :stream stream)))
        (if (eof-object? c) acc (loop (string-append acc c)))
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (run-pi dir env args)
  (let ((p (process-spawn (cons %gf (cons "pi" args))
             :cwd    dir
             :env    env
             :stdout 'pipe
             :stderr 'pipe
           ) ;process-spawn
        ) ;p
       ) ;
    (let* ((out (read-all p 'stdout))
           (err (read-all p 'stderr))
           (code (process-wait p 240000))
          ) ;
      (process-close p)
      (list code out err)
    ) ;let*
  ) ;let
) ;define

(define (state-file dir)
  (path->string (path-join (path dir) ".pi/blueprint.json"))
) ;define

(define (load-bp dir)
  (pi-blueprint-load-state (state-file dir))
) ;define

(define (save-bp! dir bp)
  (pi-blueprint-save-state! (state-file dir) bp)
) ;define

(define (phase-status dir phase-id)
  (pi-phase-status (pi-blueprint-find-phase (load-bp dir) phase-id))
) ;define

(define (set-phase-gate-command! dir phase-id index command)
  (let* ((bp (load-bp dir))
         (bp1
           (pi-blueprint-update-phase bp
             phase-id
             (lambda (phase)
               (pi-blueprint-set-field phase
                 "verification_gates"
                 (list->vector
                   (map
                     (lambda (g)
                       (if (equal? g (list-ref (pi-phase-gates phase) index))
                         (pi-blueprint-set-field g "command" command)
                         g
                       ) ;if
                     ) ;lambda
                     (pi-phase-gates phase)
                   ) ;map
                 ) ;list->vector
               ) ;pi-blueprint-set-field
             ) ;lambda
           ) ;pi-blueprint-update-phase
         ) ;bp1
        ) ;
    (save-bp! dir bp1)
  ) ;let*
) ;define

(define (complete-phase-one! dir)
  (let* ((bp (load-bp dir))
         (bp1 (pi-blueprint-complete-task bp "1.1"))
         (bp2 (pi-blueprint-complete-task bp1 "1.2"))
        ) ;
    (save-bp! dir bp2)
  ) ;let*
) ;define

(define (check-pure-jsonl out)
  (let ((lines (filter (lambda (s) (not (string-null? s))) (string-split out "\n"))))
    (check-true (>= (length lines) 4))
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
  ) ;let
) ;define


(define (snapshot-fixture)
  (vector->list (path-list (path %tmp-agent)))
) ;define
