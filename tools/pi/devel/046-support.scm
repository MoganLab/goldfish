;; [046] 验收脚本的纯函数、参数与门禁检查（由 tools/pi/devel/046.scm 通过
;; (load ...) 调用）。覆盖验收点 1、2、3、4、6；stub server 端到端见
;; 046-support-e2e.scm，门禁与 13 个新测试在 046.scm。

(define (%result-text res)
  (let ((content (pi-tool-result-content res)))
    (cdr (assoc 'text (vector-ref content 0)))
  ) ;let
) ;define

(define (%tool-call name arguments)
  (list (cons "type" "toolCall")
    (cons "id" "call-1")
    (cons "name" name)
    (cons "arguments" arguments)
  ) ;list
) ;define

(define (%edit-call path old new)
  (%tool-call "edit"
    (list (cons "path" path)
      (cons "edits" (list (list (cons "oldText" old) (cons "newText" new))))
    ) ;list
  ) ;%tool-call
) ;define

(define (%expect-cli-error tag cmd needle)
  (let ((res (%sh tag cmd 90)))
    (check (car res) => 1)
    (check-true (string-contains? (caddr res) needle))
  ) ;let
) ;define

(define (run-cli-args-checks!)
  (display "=== 验收点 1: 参数解析与互斥 ===\n")
  (check (pi-args-ref (parse-pi-args '("--red-green" "任务")) 'red-green) => #t)
  (check (pi-args-ref (parse-pi-args '("任务")) 'red-green) => #f)
  (check (pi-args-ref (parse-pi-args '("--red-green" "--not-a-real-flag" "x"))
           'unknown-flags
         ) ;pi-args-ref
    =>
    '("--not-a-real-flag")
  ) ;check
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--red-green"
                                               "--code-review"
                                               "x")))
  ) ;check-catch
  (check-catch 'pi-arg-error
    (pi-validate-review-flags (parse-pi-args '("--red-green" "--simplify" "x")))
  ) ;check-catch
  (check (pi-validate-review-flags (parse-pi-args '("--red-green" "任务")))
    =>
    #t
  ) ;check
  (%expect-cli-error "conflict-cr"
    "timeout 60 bin/gf pi --red-green --code-review x"
    "cannot be used with"
  ) ;%expect-cli-error
  (%expect-cli-error "conflict-sp"
    "timeout 60 bin/gf pi --red-green --simplify x"
    "cannot be used with"
  ) ;%expect-cli-error
  (%expect-cli-error "apply"
    "timeout 60 bin/gf pi --red-green --apply x"
    "not supported with --red-green"
  ) ;%expect-cli-error
  (%expect-cli-error "staged"
    "timeout 60 bin/gf pi --red-green --staged x"
    "not supported with --red-green"
  ) ;%expect-cli-error
  (%expect-cli-error "ref"
    "timeout 60 bin/gf pi --red-green --ref=HEAD x"
    "not supported with --red-green"
  ) ;%expect-cli-error
  (%expect-cli-error "notask"
    "timeout 60 bin/gf pi --red-green"
    "--red-green requires a task description"
  ) ;%expect-cli-error
  (check-true (string-contains? (cadr (%sh "help" "timeout 60 bin/gf pi --help" 90))
                "--red-green"
              ) ;string-contains?
  ) ;check-true
) ;define

(define (run-prompt-checks!)
  (display "=== 验收点 2: 阶段提示词注入 ===\n")
  (pi-tdd-start! "给 calc 增加 is_even")
  (let ((p (pi-build-red-green-prompt (pi-tdd-current-state))))
    (check-true (string? p))
    (check-true (string-contains? p "TDD: RED Phase"))
    (check-true (string-contains? p "给 calc 增加 is_even"))
    (check-true (string-contains? p "Verification Gate"))
    (check-true (string-contains? p "FAIL because the feature is not yet implemented")
    ) ;check-true
  ) ;let
  (let-values (((green w) (pi-tdd-advance-phase (pi-tdd-current-state) 'tests-fail)))
    (pi-tdd-set-current-state! green)
    (let ((p (pi-build-red-green-prompt (pi-tdd-current-state))))
      (check-true (string-contains? p "TDD: GREEN Phase"))
      (check-true (string-contains? p "SIMPLEST"))
      (check-true (string-contains? p "RUN tests"))
    ) ;let
  ) ;let-values
  (let-values (((refactor w) (pi-tdd-advance-phase (pi-tdd-current-state) 'tests-pass)))
    (pi-tdd-set-current-state! refactor)
    (let ((p (pi-build-red-green-prompt (pi-tdd-current-state))))
      (check-true (string-contains? p "TDD: REFACTOR Phase"))
      (check-true (string-contains? p "optional"))
    ) ;let
  ) ;let-values
  (let ((turn-prompt (pi-tdd-turn-system-prompt "BASE")))
    (check-true (string-starts? (turn-prompt '()) "BASE"))
    (check-true (string-contains? (turn-prompt '()) "REFACTOR Phase"))
  ) ;let
  (pi-tdd-set-current-state! #f)
) ;define

(define (run-detect-checks!)
  (display "=== 验收点 3: 测试运行检测与自动推进（抽样） ===\n")
  (check (pi-tdd-test-run-trigger (pi-tdd-parse-test-run-result "pytest: 1 failed, 2 passed" 1)
         ) ;pi-tdd-test-run-trigger
    =>
    'tests-fail
  ) ;check
  (check (pi-tdd-test-run-trigger (pi-tdd-parse-test-run-result "6 passed" 0))
    =>
    'tests-pass
  ) ;check
  (pi-tdd-set-current-state! (pi-tdd-initial-state "任务"))
  (pi-tdd-set-current-state! (pi-tdd-set-last-test-run (pi-tdd-current-state)
                               '((passed . 0)
                                 (failed . 2)
                                 (errors . 0)
                                 (exit-code . 1)
                                 (turn-index . 0))
                             ) ;pi-tdd-set-last-test-run
  ) ;pi-tdd-set-current-state!
  (let-values (((next warning) (pi-tdd-advance-phase (pi-tdd-current-state) 'tests-fail)))
    (check (pi-tdd-state-phase next) => 'green)
    (check warning => #f)
  ) ;let-values
  (pi-tdd-set-current-state! #f)
) ;define

(define (run-gate-checks!)
  (display "=== 验收点 4: RED 阶段写入门禁（edit 实现文件被拒） ===\n"
  ) ;display
  (%cleanup!)
  (path-write-text (path %impl) "def add(a, b):\n    return a - b\n")
  (path-write-text (path %test-file) "def test_add():\n    assert False\n")
  (pi-tdd-start! "修复 add")
  (let* ((msg (pi-execute-one-tool-call (%edit-call %impl "return a - b" "return a + b")
                #f
                30
              ) ;pi-execute-one-tool-call
         ) ;msg
         (text (pi-extract-tool-result-text msg))
        ) ;
    (check (pi-tool-result-message-is-error msg) => #t)
    (check-true (string-contains? text "TDD RED phase violation"))
    (check-true (string-contains? text "Write a failing test first"))
    (check-true (string-contains? (path-read-text %impl) "return a - b"))
    (check-false (string-contains? (path-read-text %impl) "return a + b"))
  ) ;let*
  (let ((msg (pi-execute-one-tool-call (%edit-call %test-file "assert False" "assert True")
               #f
               30
             ) ;pi-execute-one-tool-call
        ) ;msg
       ) ;
    (check (pi-tool-result-message-is-error msg) => #f)
    (check-true (string-contains? (path-read-text %test-file) "assert True"))
    (check (member %test-file (pi-tdd-state-test-files (pi-tdd-current-state)))
      =>
      (list %test-file)
    ) ;check
  ) ;let
  (let ((advance (execute-tool-call "tdd_advance" '((reason . "manual")))))
    (check (pi-tool-result-is-error advance) => #f)
    (check (pi-tdd-state-phase (pi-tdd-current-state)) => 'green)
  ) ;let
  (let ((msg (pi-execute-one-tool-call (%edit-call %impl "return a - b" "return a + b")
               #f
               30
             ) ;pi-execute-one-tool-call
        ) ;msg
       ) ;
    (check (pi-tool-result-message-is-error msg) => #f)
    (check-true (string-contains? (path-read-text %impl) "return a + b"))
    (check (member %impl (pi-tdd-state-impl-files (pi-tdd-current-state)))
      =>
      (list %impl)
    ) ;check
  ) ;let
  (pi-tdd-set-current-state! #f)
  (%cleanup!)
) ;define

(define (run-tools-checks!)
  (display "=== 验收点 6: tdd_status / tdd_advance / tdd_reset 逃生舱 ===\n"
  ) ;display
  (check-true (pi-tool-definition? (get-tool-definition "tdd_status")))
  (check-true (pi-tool-definition? (get-tool-definition "tdd_advance")))
  (check-true (pi-tool-definition? (get-tool-definition "tdd_reset")))
  (check (pi-tool-definition-name (get-tool-definition "tdd_status"))
    =>
    "tdd_status"
  ) ;check
  (pi-tdd-start! "实现登录")
  (let* ((res (execute-tool-call "tdd_status" '())) (text (%result-text res)))
    (check (pi-tool-result? res) => #t)
    (check (pi-tool-result-is-error res) => #f)
    (check-true (string-contains? text "Phase: red"))
    (check-true (string-contains? text "Task: 实现登录"))
  ) ;let*
  (let* ((res (execute-tool-call "tdd_advance" '((reason . "auto-detect missed"))))
         (text (%result-text res))
        ) ;
    (check (pi-tool-result-is-error res) => #f)
    (check text => "Advanced to GREEN phase.")
    (check (pi-tdd-state-phase (pi-tdd-current-state)) => 'green)
  ) ;let*
  (let* ((res (execute-tool-call "tdd_reset" '())) (text (%result-text res)))
    (check (pi-tool-result-is-error res) => #f)
    (check-true (string-contains? text "TDD reset to idle"))
    (check (pi-tdd-state-phase (pi-tdd-current-state)) => 'idle)
  ) ;let*
  (let ((res (execute-tool-call "tdd_advance" '())))
    (check (pi-tool-result-is-error res) => #t)
    (check-true (string-contains? (%result-text res) "No active TDD session"))
  ) ;let
  (check (pi-tdd-extend-tool-names '("read")
           (parse-pi-args '("--red-green" "x" "--exclude-tools"
                            "tdd_status,tdd_advance,tdd_reset")
           ) ;parse-pi-args
         ) ;pi-tdd-extend-tool-names
    =>
    '("read")
  ) ;check
  (pi-tdd-set-current-state! #f)
) ;define
