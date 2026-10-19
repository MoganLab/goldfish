;; [045] 验收脚本的纯函数检查（由 tools/pi/devel/045.scm 通过 (load ...) 调用）
;; 覆盖验收点 1-5、7、8 的纯逻辑部分：参数解析、变更行范围收集、Scope 措辞、
;; 提示词构造、建议解析与降级、报告格式化。
;; CLI、真实冒烟、门禁与回归在 045.scm 中。

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

(define (simplify-file-entry path status ranges available? . maybe-content)
  (append (list (cons 'path path)
            (cons 'status status)
            (cons 'ranges ranges)
            (cons 'ranges-available? available?)
          ) ;list
    (if (pair? maybe-content) (list (cons 'content (car maybe-content))) '())
  ) ;append
) ;define

(define (suggestion file line category message . maybe-suggestion)
  (append (list (cons 'file file)
            (cons 'line line)
            (cons 'category category)
            (cons 'message message)
          ) ;list
    (if (pair? maybe-suggestion)
      (list (cons 'suggestion (car maybe-suggestion)))
      '()
    ) ;if
  ) ;append
) ;define

(define (run-args-and-ranges-checks!)
  (display "=== 验收点 1: CLI 参数解析与冲突校验 ===\n")
  (let* ((p (parse-pi-args '("--simplify" "--staged" "--ref=main" "a.ts" "b.py"))))
    (check (pi-args-ref p 'simplify) => #t)
    (check (pi-args-ref p 'apply) => #f)
    (check (pi-args-ref p 'staged) => #t)
    (check (pi-args-ref p 'review-ref) => "main")
    (check (pi-args-ref p 'messages) => '("a.ts" "b.py"))
    (check (pi-validate-review-flags p) => #t)
  ) ;let*
  (let ((p (parse-pi-args '("--simplify" "--apply"))))
    (check (pi-args-ref p 'simplify) => #t)
    (check (pi-args-ref p 'apply) => #t)
    (check (pi-validate-review-flags p) => #t)
  ) ;let
  (check (pi-args-ref (parse-pi-args '("--simplify" "--ref" "v1")) 'review-ref)
    =>
    "v1"
  ) ;check
  (check (pi-args-ref (parse-pi-args '("--simplify" "--simplifier")) 'unknown-flags)
    =>
    '("--simplifier")
  ) ;check
  (check (pi-validate-review-flags (parse-pi-args '("--code-review" "--staged")))
    =>
    #t
  ) ;check
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--apply")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--simplify" "--code-review")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--staged")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--ref=main")))
  ) ;check-catch

  (display "=== 验收点 2: 变更文件与改动行范围收集 ===\n")
  (check (pi-parse-changed-ranges "@@ -99,2 +100,3 @@\n@@ -104 +105,2 @@\n")
    =>
    '(((start . 100) (end . 102)) ((start . 105) (end . 106)))
  ) ;check
  (check (pi-parse-changed-ranges "@@ -1 +10,2 @@\n@@ -5 +12,3 @@\n")
    =>
    '(((start . 10) (end . 14)))
  ) ;check
  (check (pi-parse-changed-ranges "@@ -200 +202,0 @@\n") => '())
  (check (pi-diff-ranges-command "HEAD" #f "src/a.ts")
    =>
    "git diff --unified=0 --no-ext-diff HEAD -- src/a.ts"
  ) ;check
  (check (pi-diff-ranges-command "HEAD" #t "src/a.ts")
    =>
    "git diff --unified=0 --no-ext-diff --cached -- src/a.ts"
  ) ;check
  (check (pi-diff-ranges-command "HEAD~1" #t "src/a.ts")
    =>
    "git diff --unified=0 --no-ext-diff HEAD~1 -- src/a.ts"
  ) ;check
  (check-true (string-contains? (pi-diff-ranges-command "main; rm -rf /tmp/x" #f "a b.ts")
                "'main; rm -rf /tmp/x'"
              ) ;string-contains?
  ) ;check-true
  (let* ((runner (make-runner (list (list "git diff --name-status HEAD" "M\tsrc/a.ts\nA\tsrc/new.ts\n" #f)
                                (list "git diff --unified=0 --no-ext-diff HEAD -- src/a.ts"
                                  "@@ -1 +2,2 @@\n@@ -10 +20 @@\n"
                                  #f
                                ) ;list
                              ) ;list
                 ) ;make-runner
         ) ;runner
         (res (pi-collect-simplify-files :runner runner))
         (files (%field 'files res))
        ) ;
    (check (%field 'source res) => "diff")
    (check (length files) => 2)
    (check (%field 'status (car files)) => "modified")
    (check (%field 'ranges (car files))
      =>
      '(((start . 2) (end . 3)) ((start . 20) (end . 20)))
    ) ;check
    (check (%field 'ranges-available? (cadr files)) => #t)
    (check (%field 'ranges (cadr files)) => #f)
  ) ;let*
  (let* ((runner (make-runner (list (list "git diff --name-status HEAD" "" #t)
                                (list "git diff --name-status HEAD~1" "M\ta.ts\n" #f)
                                (list "git diff --unified=0 --no-ext-diff HEAD~1 -- a.ts" "@@ -1 +3,4 @@\n" #f)
                              ) ;list
                 ) ;make-runner
         ) ;runner
         (res (pi-collect-simplify-files :runner runner :staged #t))
        ) ;
    (check (%field 'source res) => "fallback")
    (check (%field 'ranges (car (%field 'files res))) => '(((start . 3)
                                                            (end . 6))))
  ) ;let*
  (let* ((runner (make-runner (list (list "git diff --name-status HEAD" "M\ta.ts\n" #f)
                                (list "git diff --unified=0 --no-ext-diff HEAD -- a.ts"
                                  "fatal: bad revision"
                                  #t
                                ) ;list
                              ) ;list
                 ) ;make-runner
         ) ;runner
         (res (pi-collect-simplify-files :runner runner))
         (file (car (%field 'files res)))
        ) ;
    (check (%field 'ranges file) => #f)
    (check (%field 'ranges-available? file) => #f)
  ) ;let*
  (let* ((runner (make-runner (list (list "git diff --unified=0 --no-ext-diff HEAD -- a.ts" "@@ -1 +8 @@\n" #f)
                              ) ;list
                 ) ;make-runner
         ) ;runner
         (res (pi-collect-simplify-files :runner runner :files '("a.ts")))
         (file (car (%field 'files res)))
        ) ;
    (check (%field 'source res) => "explicit")
    (check (%field 'status file) => "modified")
    (check (%field 'ranges file) => '(((start . 8) (end . 8))))
  ) ;let*
  (let* ((many
           (apply string-append
             (map (lambda (i) (string-append "M\tf" (number->string i) ".ts\n"))
               '(1 2 3 4 5 6 7 8 9 10 11 12)
             ) ;map
           ) ;apply
         ) ;many
         (runner (make-runner (list (list "git diff --name-status HEAD" many #f))))
         (res (pi-collect-simplify-files :runner runner))
        ) ;
    (check (length (%field 'files res)) => 10)
    (check (%field 'message res) => "Reviewing first 10 of 12 changed files.")
  ) ;let*

) ;define

(define (run-unit-acceptance!)
  (run-args-and-ranges-checks!)
  (run-scope-and-prompt-checks!)
  (run-parse-and-format-checks!)
) ;define
