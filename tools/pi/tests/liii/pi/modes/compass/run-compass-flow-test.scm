(import (liii check)
        (liii string)
        (liii path)
        (liii os)
        (liii pi modes compass)
        (liii pi modes)
        (liii pi args)
        (liii pi compass args)
) ;import

(check-set-mode! 'report-failed)

(define dir (path-join (path-temp-dir) "gf-compass-flow-test"))
(define dir-str (path->string dir))
(define src-a (path->string (path-join dir "src" "a.js")))
(define readme (path->string (path-join dir "README.md")))
(define codemap-file (path->string (path-join dir ".pi" "codemap.json")))

(define (cleanup!)
  (for-each
    (lambda (f) (when (path-file? (path f)) (path-unlink (path f) #t)))
    (list codemap-file src-a readme)
  ) ;for-each
  (for-each
    (lambda (d) (when (path-dir? (path d)) (path-rmdir (path d))))
    (list (path->string (path-join dir ".pi"))
      (path->string (path-join dir "src"))
      dir-str
    ) ;list
  ) ;for-each
) ;define

(define (setup!)
  (cleanup!)
  (path-mkdir dir :parents #t :exist-ok #t)
  (path-mkdir (path (path->string (path-join dir "src"))) :parents #t :exist-ok #t)
  (path-write-text (path src-a) "// a\n")
  (path-write-text (path readme) "# demo\n")
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (setup!)
    (let ((orig (getcwd)))
      (dynamic-wind (lambda () (chdir dir-str))
        (lambda ()
          ;; run-compass-flow：--codemap 直接输出 Markdown，不调用模型、不需要凭据。
          (check (run-compass-flow '(("codemap" . #t)) 'json) => 0)
          (check (run-compass-flow '(("tour" . #t)) 'json) => 0)
          (check (run-compass-flow '(("tour" . "src")) 'json) => 0)
          (check (run-compass-flow '(("tour" . "no-such-topic-xyz")) 'json) => 0)

          ;; 真实解析后的参数进入 compass 本地流程。
          (check (run-compass-flow (parse-pi-args '("--tour" "src")) 'json) => 0)
          (check (run-compass-flow (parse-pi-args '("--tour")) 'json) => 0)
        ) ;lambda
        (lambda () (chdir orig))
      ) ;dynamic-wind
    ) ;let
  ) ;lambda
  (lambda () (cleanup!))
) ;dynamic-wind

(check-report)
