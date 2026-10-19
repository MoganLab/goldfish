(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi)
  (liii pi args)
) ;import

(check-set-mode! 'report-failed)

;; [044] 验收脚本：code-review 内置命令
;; 逐条对应 tools/pi/devel/044.md 验收点 1-9。
;; 验收点 1-7 的纯函数检查见 044-support.scm；本文件负责 CLI、真实冒烟与门禁。

(define %repo-root (path->string (path-cwd)))

(define %tmp-files '())

(define %clean-repo "tools/pi/devel/tmp-044-clean-repo")

(load "tools/pi/devel/044-support.scm")

(define (%sh tag cmd timeout)
  (let* ((out (string-append "tools/pi/devel/tmp-044-" tag ".out"))
         (err (string-append "tools/pi/devel/tmp-044-" tag ".err"))
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
  (for-each (lambda (f) (when (file-exists? f) (delete-file f))) %tmp-files)
  (when (file-exists? %clean-repo)
    (run (list (pi-find-bash-path) "-c" (string-append "rm -rf " %clean-repo))
      :stdin   'null
      :timeout 60
    ) ;run
  ) ;when
  (set! %tmp-files '())
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (run-unit-acceptance!)

    (display "=== 验收点 8a: CLI 错误与无变更路径 ===\n")
    (let ((res (%sh "conflict" "timeout 60 bin/gf pi --code-review --auto-review x" 90)))
      (check (car res) => 1)
      (check-true (string-contains? (caddr res) "cannot be used together"))
    ) ;let
    (check (%ok? "staged-alone" "timeout 60 bin/gf pi --staged" 90) => 1)
    (check (%ok? "ref-alone" "timeout 60 bin/gf pi --ref=main" 90) => 1)
    (let ((res (%sh "nocred"
                 (string-append "env -u GOOGLE_API_KEY -u GOOGLE_GENAI_API_KEY"
                   " PI_CODING_AGENT_DIR=" %repo-root
                   "/tools/pi/tests/fixtures/code-review/no-auth"
                   " timeout 60 bin/gf pi --code-review --no-session"
                 ) ;string-append
                 90
               ) ;%sh
          ) ;res
         ) ;
      (check (car res) => 1)
      (check-true (string-contains? (caddr res) "No credentials found"))
    ) ;let
    (check (%ok? "mk-repo"
             (string-append "rm -rf " %clean-repo " && mkdir -p " %clean-repo
               " && cd " %clean-repo
               " && git init -q && git config user.email t@example.com"
               " && git config user.name Tester && echo hello > a.txt"
               " && git add a.txt && git commit -qm init"
             ) ;string-append
             60
           ) ;%ok?
      =>
      0
    ) ;check
    (let* ((cmd (string-append "cd " %clean-repo " && timeout 60 " %repo-root
                  "/bin/gf pi --code-review --no-session --provider deepseek"
                ) ;string-append
           ) ;cmd
           (res (%sh "nochanged" cmd 90))
          ) ;
      (check (car res) => 0)
      (check-true (string-contains? (cadr res) "No changed files found"))
    ) ;let*

    (display "=== 验收点 8b: 真实审查冒烟（只读、结构化报告） ===\n"
    ) ;display
    (let* ((before (path-read-text "tools/pi/liii/pi/args.scm"))
           (cmd (string-append "timeout 280 bin/gf pi --code-review --ref=HEAD~1"
                  " --no-session --provider deepseek --model deepseek-v4-flash"
                  " --timeout 120"
                ) ;string-append
           ) ;cmd
           (res (%sh "smoke" cmd 290))
           (out (cadr res))
           (err (caddr res))
          ) ;
      (check (car res) => 0)
      (check-true (or (string-contains? out "## Code Review Findings (")
                    (string-contains? out "Code review complete: no issues found.")
                  ) ;or
      ) ;check-true
      (check-false (string-contains? out "parse-error"))
      (check-false (string-contains? err "Authorization"))
      (check-false (string-contains? err "api-key"))
      (check (path-read-text "tools/pi/liii/pi/args.scm") => before)
    ) ;let*

    (display "=== 验收点 9: 门禁、测试与工程规范 ===\n")
    (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
    (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
    (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
    (check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)
    (check (%ok? "lines"
             (string-append "for f in tools/pi/liii/pi/code-review.scm"
               " tools/pi/liii/pi/code-review/auto.scm"
               " tools/pi/liii/pi/code-review/files.scm"
               " tools/pi/liii/pi/code-review/format.scm"
               " tools/pi/liii/pi/code-review/language.scm"
               " tools/pi/liii/pi/code-review/parse.scm"
               " tools/pi/liii/pi/code-review/prompt.scm"
               " tools/pi/liii/pi/modes.scm" " tools/pi/liii/pi/modes/flow.scm"
               " tools/pi/liii/pi/modes/review.scm"
               " tools/pi/liii/pi/agent.scm"
               " tools/pi/devel/044.scm tools/pi/devel/044-support.scm;"
               " do [ $(wc -l < \"$f\") -le 300 ] || { echo \"$f\"; exit 1; }; done"
             ) ;string-append
             30
           ) ;%ok?
      =>
      0
    ) ;check
    (for-each
      (lambda (file)
        (check (%ok? "unit" (string-append "timeout 120 bin/gf test " file) 150) => 0)
      ) ;lambda
      '("tools/pi/tests/liii/pi/args/pi-parse-pi-args-review-test.scm"
        "tools/pi/tests/liii/pi/code-review/auto/pi-build-review-injection-test.scm"
        "tools/pi/tests/liii/pi/code-review/files/pi-changed-files-command-test.scm"
        "tools/pi/tests/liii/pi/code-review/files/pi-collect-changed-files-test.scm"
        "tools/pi/tests/liii/pi/code-review/files/pi-parse-changed-files-test.scm"
        "tools/pi/tests/liii/pi/code-review/files/pi-review-file-records-test.scm"
        "tools/pi/tests/liii/pi/code-review/format/pi-format-review-findings-test.scm"
        "tools/pi/tests/liii/pi/code-review/language/pi-build-language-section-test.scm"
        "tools/pi/tests/liii/pi/code-review/language/pi-detect-language-test.scm"
        "tools/pi/tests/liii/pi/code-review/language/pi-language-checklist-test.scm"
        "tools/pi/tests/liii/pi/code-review/parse/pi-parse-review-findings-test.scm"
        "tools/pi/tests/liii/pi/code-review/prompt/pi-build-review-prompt-test.scm"
        "tools/pi/tests/liii/pi/modes/flow/pi-auto-review-system-prompt-test.scm"
        "tools/pi/tests/liii/pi/agent/pi-resolve-turn-system-prompt-test.scm")
    ) ;for-each
    (for-each
      (lambda (file)
        (check (%ok? "regress" (string-append "timeout 120 bin/gf test " file) 150)
          =>
          0
        ) ;check
      ) ;lambda
      '("tools/pi/tests/liii/pi-cli-e2e-test.scm"
        "tools/pi/tests/liii/pi-json-e2e-test.scm")
    ) ;for-each
    (display "=== 044 验收完成 ===\n")
  ) ;lambda
  (lambda () (%cleanup!) (check-report))
) ;dynamic-wind
