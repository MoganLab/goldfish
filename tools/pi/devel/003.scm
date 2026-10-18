(import (scheme base)
  (scheme file)
  (liii check)
  (liii subprocess)
  (liii json)
  (liii string)
  (liii list)
  (liii os)
  (liii path)
  (liii sort)
) ;import

(check-set-mode! 'report-failed)

;; [003] 核心库迁移·模块名统一 验收脚本
(set! *load-path* (cons "tools/pi" *load-path*))

(define (can-import? lib)
  (catch #t (lambda () (eval `(import ,lib) (rootlet)) #t) (lambda args #f))
) ;define

(define (has-export? file-path)
  (and (file-exists? file-path)
    (guard (e (else #f))
      (call-with-input-file file-path
        (lambda (port)
          (let loop
            ((form (read port)))
            (cond ((eof-object? form) #f)
                  ((and (pair? form) (eq? (car form) 'define-library))
                   (let check-body
                     ((body (cddr form)))
                     (cond ((null? body) #f)
                           ((and (pair? (car body)) (eq? (caar body) 'export)) #t)
                           (else (check-body (cdr body)))
                     ) ;cond
                   ) ;let
                  ) ;
                  (else (loop (read port)))
            ) ;cond
          ) ;let
        ) ;lambda
      ) ;call-with-input-file
    ) ;guard
  ) ;and
) ;define

(define (file-line-count p)
  (call-with-input-file (if (string? p) p (path->string p))
    (lambda (port)
      (let loop
        ((count 0))
        (let ((line (read-line port)))
          (if (eof-object? line) count (loop (+ count 1)))
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-input-file
) ;define

;; a) tools/pi/liii/ 根目录只有 pi.scm 与 pi/ 目录，全仓库无 (liii pi-xxx) 平铺残留
(let* ((entries (vector->list (listdir "tools/pi/liii")))
       (filtered
         (filter
           (lambda (s) (not (or (string=? s ".") (string=? s ".."))))
           entries
         ) ;filter
       ) ;filtered
      ) ;
  (check (list-sort string<? filtered) => '("pi" "pi.scm"))
) ;let*

(for-each (lambda (f) (check-false (file-exists? f)))
  '("tools/pi/liii/goldpi.scm" "tools/pi/liii/goldpi-args.scm"
    "tools/pi/liii/goldpi-config.scm" "tools/pi/liii/goldpi-provider.scm"
    "tools/pi/liii/goldpi-agent.scm" "tools/pi/liii/goldpi-tools.scm"
    "tools/pi/liii/goldpi-session.scm" "tools/pi/liii/goldpi-stream.scm"
    "tools/pi/liii/goldpi-stream-parse.scm" "tools/pi/liii/goldpi-anthropic.scm"
    "tools/pi/liii/goldpi-anthropic-stream.scm"
    "tools/pi/liii/goldpi-anthropic-parse.scm" "tools/pi/liii/goldpi-help.scm"
    "tools/pi/liii/pi-agent.scm" "tools/pi/liii/pi-args.scm"
    "tools/pi/liii/pi-config.scm" "tools/pi/liii/pi-help.scm"
    "tools/pi/liii/pi-provider.scm" "tools/pi/liii/pi-session.scm"
    "tools/pi/liii/pi-tools.scm" "tools/pi/liii/pi-stream.scm"
    "tools/pi/liii/pi-stream-parse.scm" "tools/pi/liii/pi-anthropic.scm"
    "tools/pi/liii/pi-anthropic-parse.scm"
    "tools/pi/liii/pi-anthropic-stream.scm" "tools/pi/liii/pi-cli.scm")
) ;for-each

(check (run "git grep -E '\\(define-library[[:space:]]+\\(liii[[:space:]]+pi-[a-zA-Z0-9_-]+\\)' goldfish/ tools/ tests/"
         :timeout 30
       ) ;run
  =>
  1
) ;check
(check (run "git grep -E '\\(liii pi-[a-zA-Z0-9_-]+\\)' goldfish/ tools/pi/liii/ tools/pi/tests/ tests/"
         :timeout 30
       ) ;run
  =>
  1
) ;check

;; b) 13 个二级子库与 CLI 入口 pi/cli.scm 文件存在且可 import，二级子库具备 export 层

(define *target-sublibraries*
  '(("tools/pi/liii/pi/types.scm" (liii pi types) #t)
    ("tools/pi/liii/pi/gemini.scm" (liii pi gemini) #t)
    ("tools/pi/liii/pi/anthropic.scm" (liii pi anthropic) #t)
    ("tools/pi/liii/pi/auth.scm" (liii pi auth) #t)
    ("tools/pi/liii/pi/models.scm" (liii pi models) #t)
    ("tools/pi/liii/pi/retry.scm" (liii pi retry) #t)
    ("tools/pi/liii/pi/agent.scm" (liii pi agent) #t)
    ("tools/pi/liii/pi/tools.scm" (liii pi tools) #t)
    ("tools/pi/liii/pi/config.scm" (liii pi config) #t)
    ("tools/pi/liii/pi/prompt.scm" (liii pi prompt) #t)
    ("tools/pi/liii/pi/session.scm" (liii pi session) #t)
    ("tools/pi/liii/pi/args.scm" (liii pi args) #t)
    ("tools/pi/liii/pi/modes.scm" (liii pi modes) #t)
    ("tools/pi/liii/pi/cli.scm" (liii pi cli) #f))
) ;define

(for-each
  (lambda (item)
    (let ((file (car item)) (lib (cadr item)) (is-sublib? (caddr item)))
      (check-true (file-exists? file))
      (check-true (can-import? lib))
      (if is-sublib? (check-true (has-export? file)))
    ) ;let
  ) ;lambda
  *target-sublibraries*
) ;for-each

(check-true (can-import? '(liii pi)))

;; c) gfproject.json 指向 (liii pi cli)
(check
  (guard (e (else #f))
    (let* ((text
             (call-with-input-file "gfproject.json"
               (lambda (p)
                 (let loop
                   ((acc '()))
                   (let ((c (read-char p)))
                     (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc)))
                   ) ;let
                 ) ;let
               ) ;lambda
             ) ;call-with-input-file
           ) ;text
           (json (string->json text))
           (tools
             (and (pair? json)
               (cond ((assoc "tools" json) => cdr)
                     ((assoc 'tools json) => cdr)
                     (else #f)
               ) ;cond
             ) ;and
           ) ;tools
           (pi-tool
             (and (pair? tools)
               (cond ((assoc "pi" tools) => cdr)
                     ((assoc 'pi tools) => cdr)
                     (else #f)
               ) ;cond
             ) ;and
           ) ;pi-tool
           (module-name
             (and (pair? pi-tool)
               (cond ((assoc "module" pi-tool) => cdr)
                     ((assoc 'module pi-tool) => cdr)
                     (else #f)
               ) ;cond
             ) ;and
           ) ;module-name
           (org-name
             (and (pair? pi-tool)
               (cond ((assoc "organization" pi-tool) => cdr)
                     ((assoc 'organization pi-tool) => cdr)
                     (else #f)
               ) ;cond
             ) ;and
           ) ;org-name
          ) ;
      (and (string=? module-name "pi/cli")
        (string=? org-name "liii")
        (not (string-contains text "goldpi"))
        (not (string-contains text "pi-cli"))
      ) ;and
    ) ;let*
  ) ;guard
  =>
  #t
) ;check

;; d) 单文件均不超 300 行（bin/gf check）
(check (run "bin/gf check" :timeout 120) => 0)
(for-each
  (lambda (p) (check (<= (file-line-count p) 300) => #t))
  (path-rglob "tools/pi/liii" "*.scm")
) ;for-each

;; e) bin/gf test 全过
(for-each
  (lambda (test-file)
    (check (run (string-append "bin/gf test " test-file) :timeout 60) => 0)
  ) ;lambda
  '("tools/pi/tests/liii/pi-args-test.scm"
    "tools/pi/tests/liii/pi-config-test.scm"
    "tools/pi/tests/liii/pi-provider-test.scm"
    "tools/pi/tests/liii/pi-agent-test.scm"
    "tools/pi/tests/liii/pi-cli-e2e-test.scm"
    "tools/pi/tests/liii/pi-json-e2e-test.scm"
    "tools/pi/tests/liii/pi-prompt-test.scm"
    "tools/pi/tests/liii/pi-session-test.scm"
    "tools/pi/tests/liii/pi-tools-basic-test.scm"
    "tools/pi/tests/liii/pi-tools-search-test.scm"
    "tools/pi/tests/liii/pi-event-test.scm"
    "tools/pi/tests/liii/pi-anthropic-test.scm"
    "tools/pi/tests/liii/pi-test.scm")
) ;for-each

(check (run "sh -c 'for f in tools/pi/tests/liii/pi/*-test.scm; do bin/gf test \"$f\" || exit 1; done'"
         :timeout 180
       ) ;run
  =>
  0
) ;check

;; f) bin/gf pi --help 与 doc 抽查可用
(check (run "bin/gf pi --help" :timeout 30) => 0)
(check (run "bin/gf doc --build-json" :timeout 60) => 0)
(check (run "bin/gf doc substring" :timeout 30) => 0)
(check (run "bin/gf doc scheme/base" :timeout 30) => 0)

(check-report)
