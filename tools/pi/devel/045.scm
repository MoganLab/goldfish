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

;; [045] 验收脚本：simplify 内置命令
;; 逐条对应 tools/pi/devel/045.md 验收点 1-9。
;; 验收点 1-5、7 的纯函数检查见 045-support.scm；本文件负责 CLI、真实冒烟与门禁。

(define %repo-root (path->string (path-cwd)))

(define %tmp-files '())

(define %sandbox "tools/pi/devel/tmp-045-sandbox")

(define %clean-repo "tools/pi/devel/tmp-045-clean-repo")

(define %sample-clean
  "function compute(items: number[]) {\n  let total = 0;\n  for (const item of items) {\n    total = total + item;\n  }\n  return total;\n}\n"
) ;define

(define %sample-messy
  "function compute(items: number[]) {\n  let total = 0;\n  for (const item of items) {\n    total = total + item;\n  }\n  return total;\n}\n\nfunction computeTotalWithDiscount(items: number[], discount: number) {\n  let t = 0;\n  for (const item of items) {\n    t = t + item;\n  }\n  if (t > 100) {\n    t = t * 1.0;\n  }\n  return t - discount;\n}\n"
) ;define

(load "tools/pi/devel/045-support.scm")
(load "tools/pi/devel/045-support-prompt.scm")
(load "tools/pi/devel/045-support-report.scm")

(define (%sh tag cmd timeout)
  (let* ((out (string-append "tools/pi/devel/tmp-045-" tag ".out"))
         (err (string-append "tools/pi/devel/tmp-045-" tag ".err"))
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

(define (%sample-path)
  (string-append %repo-root "/" %sandbox "/sample.ts")
) ;define

(define (%cleanup!)
  (for-each (lambda (f) (when (file-exists? f) (delete-file f))) %tmp-files)
  (for-each
    (lambda (dir)
      (when (file-exists? dir)
        (run (list (pi-find-bash-path) "-c" (string-append "rm -rf " dir))
          :stdin   'null
          :timeout 60
        ) ;run
      ) ;when
    ) ;lambda
    (list %sandbox %clean-repo)
  ) ;for-each
  (set! %tmp-files '())
) ;define

(define (%make-sandbox!)
  (check (%ok? "sandbox-init"
           (string-append "rm -rf " %sandbox " && mkdir -p " %sandbox " && cd "
             %sandbox " && git init -q && git config user.email t@example.com"
             " && git config user.name Tester"
           ) ;string-append
           60
         ) ;%ok?
    =>
    0
  ) ;check
  (path-write-text (path (%sample-path)) %sample-clean)
  (check (%ok? "sandbox-commit"
           (string-append "cd " %sandbox " && git add sample.ts && git commit -qm init")
           60
         ) ;%ok?
    =>
    0
  ) ;check
  (path-write-text (path (%sample-path)) %sample-messy)
) ;define

(define (%sha tag)
  (let ((res (%sh tag (string-append "sha256sum " %sandbox "/sample.ts") 30)))
    (check (car res) => 0)
    (string-trim-both (cadr res))
  ) ;let
) ;define

(define (%run-smoke tag extra)
  (%sh tag
    (string-append "cd " %sandbox " && timeout 260 " %repo-root
      "/bin/gf pi --simplify " extra
      " --no-session --provider deepseek --model deepseek-v4-flash"
      " --timeout 120"
    ) ;string-append
    290
  ) ;%sh
) ;define

(define (%check-read-only-smoke!)
  (display "=== 验收点 8b: 真实只读冒烟（建议报告 + 零写入） ===\n"
  ) ;display
  (let* ((before (%sha "smoke-sha-before"))
         (res (%run-smoke "smoke" ""))
         (out (cadr res))
         (err (caddr res))
         (after (%sha "smoke-sha-after"))
        ) ;
    (check (car res) => 0)
    (check-true (or (string-starts? out "## Simplify Suggestions (")
                  (string-contains? out "No simplification opportunities found.")
                ) ;or
    ) ;check-true
    (check-false (string-contains? out "parse-error"))
    (check-false (string-contains? out "Authorization"))
    (check-false (string-contains? out "api-key"))
    (check-false (string-contains? err "Authorization"))
    (check-false (string-contains? err "api-key"))
    (check after => before)
  ) ;let*
) ;define

(define (%check-apply-smoke!)
  (display "=== 验收点 6: 应用模式真实改写 ===\n")
  (let* ((before (%sha "apply-sha-before"))
         (res (%run-smoke "apply" "--apply"))
         (out (cadr res))
         (status (%sh "apply-status"
                   (string-append "cd " %sandbox " && git status --porcelain")
                   30
                 ) ;%sh
         ) ;status
        ) ;
    (check (car res) => 0)
    (check-true (> (string-length (string-trim-both out)) 0))
    (check-true (string-contains? out "computeTotalWithDiscount"))
    (check-false (string=? (%sha "apply-sha-after") before))
    (check (car status) => 0)
    (check
      (length
        (filter
          (lambda (line) (not (string-null? (string-trim-both line))))
          (string-split (cadr status) "\n")
        ) ;filter
      ) ;length
      =>
      1
    ) ;check
  ) ;let*
) ;define

(define (%check-cli-paths!)
  (display "=== 验收点 1b: CLI 错误路径与帮助 ===\n")
  (let ((res (%sh "apply-alone" "timeout 60 bin/gf pi --apply" 90)))
    (check (car res) => 1)
    (check-true (string-contains? (caddr res) "--apply requires --simplify"))
  ) ;let
  (let ((res (%sh "conflict" "timeout 60 bin/gf pi --simplify --code-review" 90)))
    (check (car res) => 1)
    (check-true (string-contains? (caddr res) "cannot be used together"))
  ) ;let
  (check (%ok? "staged-alone" "timeout 60 bin/gf pi --staged" 90) => 1)
  (check (%ok? "ref-alone" "timeout 60 bin/gf pi --ref=main" 90) => 1)
  (let ((out (cadr (%sh "help" "timeout 60 bin/gf pi --help" 90))))
    (check-true (string-contains? out "--simplify"))
    (check-true (string-contains? out "--apply"))
    (check-true (string-contains? out "With --code-review/--simplify"))
  ) ;let
  (let ((res (%sh "nocred"
               (string-append "cd " %sandbox
                 " && env -u GOOGLE_API_KEY -u GOOGLE_GENAI_API_KEY -u GEMINI_API_KEY"
                 " PI_CODING_AGENT_DIR=" %repo-root
                 "/tools/pi/tests/fixtures/code-review/no-auth" " timeout 60 "
                 %repo-root "/bin/gf pi --simplify --no-session"
               ) ;string-append
               90
             ) ;%sh
        ) ;res
       ) ;
    (check (car res) => 1)
    (check-true (string-contains? (caddr res) "No credentials found"))
  ) ;let
  (check (%ok? "mk-clean"
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
                "/bin/gf pi --simplify --no-session --provider deepseek"
              ) ;string-append
         ) ;cmd
         (res (%sh "nochanged" cmd 90))
        ) ;
    (check (car res) => 0)
    (check-true (string-contains? (cadr res) "No changed files found"))
  ) ;let*
) ;define

(define (%check-gates!)
  (display "=== 验收点 9: 门禁、测试与工程规范 ===\n")
  (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
  (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
  (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
  (check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)
  (check (%ok? "lines"
           (string-append "for f in tools/pi/liii/pi/simplify.scm"
             " tools/pi/liii/pi/simplify/format.scm"
             " tools/pi/liii/pi/simplify/parse.scm"
             " tools/pi/liii/pi/simplify/prompt.scm"
             " tools/pi/liii/pi/simplify/ranges.scm"
             " tools/pi/liii/pi/simplify/scope.scm"
             " tools/pi/liii/pi/modes/simplify.scm"
             " tools/pi/liii/pi/modes.scm tools/pi/liii/pi/args.scm"
             " tools/pi/liii/pi/help.scm tools/pi/liii/pi.scm"
             " tools/pi/liii/pi/code-review/files.scm"
             " tools/pi/devel/045.scm tools/pi/devel/045-support.scm"
             " tools/pi/devel/045-support-prompt.scm"
             " tools/pi/devel/045-support-report.scm;"
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
    '("tools/pi/tests/liii/pi/args/pi-parse-pi-args-simplify-test.scm"
      "tools/pi/tests/liii/pi/simplify/ranges/pi-parse-changed-ranges-test.scm"
      "tools/pi/tests/liii/pi/simplify/ranges/pi-diff-ranges-command-test.scm"
      "tools/pi/tests/liii/pi/simplify/ranges/pi-collect-simplify-files-test.scm"
      "tools/pi/tests/liii/pi/simplify/scope/pi-build-scope-lines-test.scm"
      "tools/pi/tests/liii/pi/simplify/prompt/pi-build-simplify-prompt-test.scm"
      "tools/pi/tests/liii/pi/simplify/prompt/pi-build-simplify-apply-prompt-test.scm"
      "tools/pi/tests/liii/pi/simplify/parse/pi-parse-simplify-suggestions-test.scm"
      "tools/pi/tests/liii/pi/simplify/format/pi-format-simplify-suggestions-test.scm")
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
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (run-unit-acceptance!)
    (check (%ok? "sandbox-reset"
             (string-append "rm -rf " %sandbox " && mkdir -p " %sandbox)
             60
           ) ;%ok?
      =>
      0
    ) ;check
    (%make-sandbox!)
    (%check-read-only-smoke!)
    (%check-apply-smoke!)
    (%check-cli-paths!)
    (%cleanup!)
    (%check-gates!)
    (display "=== 045 验收完成 ===\n")
  ) ;lambda
  (lambda () (%cleanup!) (check-report))
) ;dynamic-wind
