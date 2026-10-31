(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (string-trim-both (substring line 5 (string-length line))))
    #f
  ) ;if
) ;define

(define stub-server-file
  (if (file-exists? "tools/pi/tests/stub_gemini_server.py")
    "tools/pi/tests/stub_gemini_server.py"
    (if (file-exists? "tests/stub_gemini_server.py")
      "tests/stub_gemini_server.py"
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

(define CONFIG_EMPTY_DIR
  (if (file-exists? "tests/fixtures/config-empty")
    "tests/fixtures/config-empty"
    (if (file-exists? "tools/pi/tests/fixtures/config-empty")
      "tools/pi/tests/fixtures/config-empty"
      "../../tools/pi/tests/fixtures/config-empty"
    ) ;if
  ) ;if
) ;define

(define GF_BIN (executable))

(define %tmp-agent-dir
  (path->string (path-join (path-temp-dir) "gf-pi-cli-e2e-direct"))
) ;define

(define %tmp-empty-dir
  (path->string (path-join (path-temp-dir) "gf-pi-cli-e2e-empty"))
) ;define

(define (setup-fixtures!)
  (unless (path-exists? (path %tmp-agent-dir))
    (path-mkdir (path %tmp-agent-dir) :parents #t :exist-ok #t)
  ) ;unless
  (for-each
    (lambda (name)
      (let ((src (path-join (path FIXTURE_DIR) (path name)))
            (dst (path-join (path %tmp-agent-dir) (path name)))
           ) ;
        (when (path-exists? src)
          (path-copy src dst)
        ) ;when
      ) ;let
    ) ;lambda
    '("auth.json" "providers.json"
      "settings.json")
  ) ;for-each
  ;; 模型目录（models/<provider>.json 与 models/<provider>/<model>.json）
  (let ((src (path-join (path FIXTURE_DIR) (path "models")))
        (dst (path-join (path %tmp-agent-dir) (path "models")))
       ) ;
    (define (copy-tree s d)
      (unless (path-exists? d)
        (path-mkdir d :parents #t :exist-ok #t)
      ) ;unless
      (for-each (lambda (name)
                  (let ((sp (path-join s (path name)))
                        (dp (path-join d (path name)))
                       ) ;
                    (if (path-stat-dir? (path-stat (path->string sp)))
                      (copy-tree sp dp)
                      (path-copy sp dp)
                    ) ;if
                  ) ;let
                ) ;lambda
        (vector->list (listdir (path->string s)))
      ) ;for-each
    ) ;define
    (when (path-exists? src)
      (copy-tree src dst)
    ) ;when
  ) ;let
  (unless (path-exists? (path %tmp-empty-dir))
    (path-mkdir (path %tmp-empty-dir) :parents #t :exist-ok #t)
  ) ;unless
  (let ((src (path-join (path CONFIG_EMPTY_DIR) (path "settings.json")))
        (dst (path-join (path %tmp-empty-dir) (path "settings.json")))
       ) ;
    (when (path-exists? src)
      (path-copy src dst)
    ) ;when
  ) ;let
) ;define

(define (cleanup-fixtures!)
  (when (path-exists? (path %tmp-agent-dir))
    (rmtree %tmp-agent-dir #t)
  ) ;when
  (when (path-exists? (path %tmp-empty-dir))
    (rmtree %tmp-empty-dir #t)
  ) ;when
) ;define

(define (read-all-output p stream)
  (let loop
    ((acc ""))
    (let ((c (process-read p :stream stream)))
      (if (eof-object? c) acc (loop (string-append acc c)))
    ) ;let
  ) ;let
) ;define

(define* (safe-exec cmd env (timeout-ms 10000))
  (let* ((p (process-spawn cmd :env env :stdout 'pipe :stderr 'pipe))
         (code (process-wait p timeout-ms))
        ) ;
    (unless code
      (process-kill p)
      (process-wait p 2000)
    ) ;unless
    (let ((out (read-all-output p 'stdout)) (err (read-all-output p 'stderr)))
      (process-close p)
      (list code out err)
    ) ;let
  ) ;let*
) ;define*
