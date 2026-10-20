;; [044] 验收脚本的纯函数检查（由 tools/pi/devel/044.scm 通过 (load ...) 调用）
;; 覆盖验收点 1-7：参数解析、diff 收集、语言清单、提示词、findings 解析、
;; 报告格式化、自动审查注入。CLI/真实冒烟/门禁在 044.scm 中。

(define (%field key entry)
  (cdr (assoc key entry))
) ;define

(define (make-runner table)
  (lambda (cmd timeout)
    (let ((entry (assoc cmd table)))
      (if entry (values (cadr entry) (caddr entry)) (values "" #t))
    ) ;let
  ) ;lambda
) ;define

(define (file-entry path lang content)
  (list (cons 'path path) (cons 'language lang) (cons 'content content))
) ;define

(define (finding severity file line category message . suggestion)
  (append (list (cons 'severity severity)
            (cons 'file file)
            (cons 'line line)
            (cons 'category category)
            (cons 'message message)
          ) ;list
    (if (pair? suggestion) (list (cons 'suggestion (car suggestion))) '())
  ) ;append
) ;define

(define (run-unit-acceptance!)
  (display "=== 验收点 1: CLI 参数解析与冲突校验 ===\n")
  (let* ((p (parse-pi-args '("--code-review" "--staged" "--ref=main" "a.ts"
                             "b.py"))))
    (check (pi-args-ref p 'code-review) => #t)
    (check (pi-args-ref p 'staged) => #t)
    (check (pi-args-ref p 'review-ref) => "main")
    (check (pi-args-ref p 'messages) => '("a.ts" "b.py"))
    (check (pi-validate-review-flags p) => #t)
  ) ;let*
  (check (pi-args-ref (parse-pi-args '("--code-review" "--ref" "v1")) 'review-ref)
    =>
    "v1"
  ) ;check
  (check (pi-args-ref (parse-pi-args '("--refactor")) 'unknown-flags)
    =>
    '("--refactor")
  ) ;check
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--code-review" "--auto-review")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--staged")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--ref=main")))
  ) ;check-catch

  (display "=== 验收点 2: 变更文件收集、回退与截断 ===\n")
  (check (pi-changed-files-command "HEAD" #f) => "git diff --name-status HEAD")
  (check (pi-changed-files-command "HEAD" #t)
    =>
    "git diff --name-status --cached"
  ) ;check
  (check-true (string-contains? (pi-changed-files-command "main; rm -rf /tmp/x" #f)
                "'main; rm -rf /tmp/x'"
              ) ;string-contains?
  ) ;check-true
  (let* ((out (string-append "M\tsrc/app.ts\n" "A\tsrc/new.py\n"
                "R100\told/name.ts\tnew/name.ts\n" "C75\tsrc/a.ts\tsrc/b.ts\n"
                "D\tremoved.ts\n"
              ) ;string-append
         ) ;out
         (files (pi-parse-changed-files out))
        ) ;
    (check (length files) => 4)
    (check (%field 'path (list-ref files 2)) => "new/name.ts")
    (check (%field 'status (list-ref files 2)) => "renamed")
    (check (%field 'path (list-ref files 3)) => "src/b.ts")
  ) ;let*
  (let* ((runner (make-runner (list (list "git diff --name-status HEAD" "M\tsrc/app.ts\nA\tsrc/new.py\n" #f))
                 ) ;make-runner
         ) ;runner
         (res (pi-collect-changed-files :runner runner))
        ) ;
    (check (%field 'source res) => "diff")
    (check (%field 'paths res) => '("src/app.ts" "src/new.py"))
  ) ;let*
  (let* ((runner (make-runner (list (list "git diff --name-status HEAD" "" #t)
                                (list "git diff --name-status HEAD~1" "M\tfallback.ts\n" #f)
                              ) ;list
                 ) ;make-runner
         ) ;runner
         (res (pi-collect-changed-files :runner runner))
        ) ;
    (check (%field 'source res) => "fallback")
    (check (%field 'paths res) => '("fallback.ts"))
  ) ;let*
  (let* ((empty (make-runner '())) (res (pi-collect-changed-files :runner empty)))
    (check (%field 'paths res) => '())
  ) ;let*
  (let* ((many (string-append "M\tf01.ts\nM\tf02.ts\nM\tf03.ts\nM\tf04.ts\nM\tf05.ts\n"
                 "M\tf06.ts\nM\tf07.ts\nM\tf08.ts\nM\tf09.ts\nM\tf10.ts\n"
                 "M\tf11.ts\nM\tf12.ts\n"
               ) ;string-append
         ) ;many
         (runner (make-runner (list (list "git diff --name-status HEAD" many #f))))
         (res (pi-collect-changed-files :runner runner))
        ) ;
    (check (length (%field 'paths res)) => 10)
    (check (%field 'message res) => "Reviewing first 10 of 12 changed files.")
  ) ;let*
  (let* ((runner (make-runner '()))
         (res (pi-collect-changed-files :runner runner :files '("a.ts" "b.py")))
        ) ;
    (check (%field 'source res) => "explicit")
    (check (%field 'paths res) => '("a.ts" "b.py"))
  ) ;let*

  (display "=== 验收点 3: 语言检测与检查清单 ===\n")
  (for-each
    (lambda (pair) (check (pi-detect-language (car pair)) => (cdr pair)))
    '(("src/app.ts" . "typescript")
      ("src/app.tsx" . "typescript")
      ("scripts/run.py" . "python")
      ("cmd/main.go" . "go")
      ("src/lib.rs" . "rust")
      ("src/Main.java" . "java")
      ("web/index.php" . "php")
      ("tools/pi/liii/pi/agent.scm" . "scheme")
      ("README.md" . #f))
  ) ;for-each
  (for-each
    (lambda (lang) (check (length (pi-language-checklist lang)) => 5))
    '("typescript" "python" "go" "rust" "java" "php" "scheme")
  ) ;for-each
  (check (pi-language-checklist "cobol") => '())
  (let* ((s (pi-build-language-section '("typescript" "python" "typescript"))))
    (check-true (string-contains? s "### Typescript"))
    (check-true (string-contains? s "### Python"))
    (check (- (length (string-split s "### Typescript")) 1) => 1)
  ) ;let*

  (display "=== 验收点 4: 审查提示词构造 ===\n")
  (let* ((files (list (file-entry "src/app.ts" "typescript" "const x = 1;")
                  (file-entry "notes.txt" #f "hello")
                ) ;list
         ) ;files
         (p (pi-build-review-prompt files))
        ) ;
    (check-true (string-contains? p "Return your findings as JSON"))
    (check-true (string-contains? p "do not call or request any tools"))
    (check-true (string-contains? p "CRITICAL: Security vulnerabilities"))
    (check-true (string-contains? p "INFO: Style improvements"))
    (check-true (string-contains? p "If no issues are found, return: `{\"findings\": []}`")
    ) ;check-true
    (check-true (string-contains? p "### Typescript"))
    (check-true (string-contains? p "### src/app.ts (typescript)"))
    (check-true (string-contains? p "const x = 1;"))
    (check-false (string-contains? p "### notes.txt (typescript)"))
  ) ;let*

  (display "=== 验收点 5: findings 解析与降级 ===\n")
  (let* ((text "```json\n{\"findings\":[{\"severity\":\"HIGH\",\"file\":\"b.py\",\"line\":7,\"category\":\"error-handling\",\"message\":\"oops\",\"suggestion\":\"wrap it\"}]}\n```"
         ) ;text
         (fs (pi-parse-review-findings text))
        ) ;
    (check (length fs) => 1)
    (check (%field 'severity (car fs)) => "HIGH")
    (check (%field 'line (car fs)) => 7)
    (check (%field 'suggestion (car fs)) => "wrap it")
  ) ;let*
  (check (pi-parse-review-findings "{\"findings\":[]}") => '())
  (let* ((text "{\"findings\":[{\"severity\":\"BLOCKER\",\"file\":\"x\",\"category\":\"c\",\"message\":\"m\"},{\"severity\":\"INFO\",\"file\":\"y\",\"category\":\"c\",\"message\":\"ok\"}]}"
         ) ;text
         (fs (pi-parse-review-findings text))
        ) ;
    (check (length fs) => 1)
    (check (%field 'file (car fs)) => "y")
  ) ;let*
  (let* ((fs (pi-parse-review-findings "this is not json")))
    (check (%field 'severity (car fs)) => "INFO")
    (check (%field 'category (car fs)) => "parse-error")
  ) ;let*

  (display "=== 验收点 6: 报告格式化 ===\n")
  (check (pi-format-review-findings '())
    =>
    "Code review complete: no issues found. Code looks clean."
  ) ;check
  (let* ((fs (list (finding "INFO" "c.ts" 3 "style" "minor")
               (finding "HIGH" "b.ts" 2 "bug" "oops" "wrap it")
               (finding "CRITICAL" "a.ts" 1 "security" "bad" "fix now")
             ) ;list
         ) ;fs
         (out (pi-format-review-findings fs))
        ) ;
    (check-true (string-starts? out "## Code Review Findings (1 CRITICAL, 1 HIGH, 1 INFO)")
    ) ;check-true
    (check-true (string-contains? out "- **CRITICAL** [security] a.ts:1: bad"))
    (check-true (string-contains? out "\n  Suggestion: fix now"))
    (check-true
      (< (string-length (car (string-split out "**CRITICAL**")))
        (string-length (car (string-split out "**HIGH**")))
      ) ;<
    ) ;check-true
  ) ;let*

  (display "=== 验收点 7: 自动审查清单注入 ===\n")
  (check (pi-build-review-injection '()) => #f)
  (let* ((calls
           (list
             (list (cons "name" "write") (cons "arguments" (list (cons "path" "a.ts"))))
             (list (cons "name" "edit") (cons "arguments" (list (cons "path" "b.py"))))
             (list (cons "name" "read") (cons "arguments" (list (cons "path" "c.ts"))))
           ) ;list
         ) ;calls
         (files (pi-edited-files-from-tool-calls calls))
         (block (pi-build-review-injection files))
        ) ;
    (check (length files) => 2)
    (check (%field 'path (car files)) => "a.ts")
    (check (%field 'language (cadr files)) => "python")
    (check-true (string-starts? block "\n\n## Code Review\n\nYou edited: a.ts, b.py.")
    ) ;check-true
    (check-true (string-contains? block "### Typescript"))
    (check-true (string-contains? block "### Python"))
  ) ;let*
  (let ((provider (pi-auto-review-system-prompt "BASE")))
    (check (provider '()) => "BASE")
    (check-true
      (string-starts?
        (provider
          (list
            (list (cons "name" "write") (cons "arguments" (list (cons "path" "a.scm"))))
          ) ;list
        ) ;provider
        "BASE\n\n## Code Review"
      ) ;string-starts?
    ) ;check-true
  ) ;let
) ;define
