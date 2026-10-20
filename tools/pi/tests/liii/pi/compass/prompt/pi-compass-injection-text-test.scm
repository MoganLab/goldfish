(import (liii check)
        (liii string)
        (liii path)
        (liii pi compass project)
        (liii pi compass store)
        (liii pi compass prompt)
) ;import

(check-set-mode! 'report-failed)

(define dir (path-join (path-temp-dir) "gf-compass-inject-test"))
(define dir-str (path->string dir))
(define codemap-file (pi-compass-default-codemap-path dir-str))
(define src-a (path->string (path-join dir "src" "a.js")))
(define src-b (path->string (path-join dir "src" "b.js")))
(define readme (path->string (path-join dir "README.md")))

(define (cleanup!)
  (for-each
    (lambda (f) (when (path-file? (path f)) (path-unlink (path f) #t)))
    (list codemap-file src-a src-b readme)
  ) ;for-each
  (for-each
    (lambda (d) (when (path-dir? (path d)) (path-rmdir (path d))))
    (list (path->string (path-join dir "src"))
      (path->string (path-join dir ".pi"))
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
  (let ((project (pi-compass-detect-project dir-str)))
    (pi-compass-get-or-generate dir-str project codemap-file #f)
  ) ;let
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (setup!)
    ;; 缓存存在且哈希一致 → 注入。
    (let ((block (pi-compass-injection-text '() dir-str)))
      (check (string? block) => #t)
      (check (string-starts? block "\n\n## Codebase Map:") => #t)
    ) ;let

    ;; --no-codemap-inject 关闭注入。
    (check (pi-compass-injection-text '((no-codemap-inject . #t)) dir-str)
      =>
      #f
    ) ;check

    ;; 注入块追加到既有系统提示词之后。
    (check (string-starts?
             (pi-compass-turn-system-prompt "BASE" '() dir-str)
             "BASE\n\n## Codebase Map:"
           ) ;string-starts?
      =>
      #t
    ) ;check

    ;; 深度 2 改动 → 缓存陈旧 → 不注入。
    (path-write-text (path src-b) "// b\n")
    (check (pi-compass-injection-text '() dir-str) => #f)

    ;; 无缓存工程 → 不注入。
    (check (pi-compass-injection-text '() (path->string (path-temp-dir))) => #f)
  ) ;lambda
  (lambda () (cleanup!))
) ;dynamic-wind

(check-report)
