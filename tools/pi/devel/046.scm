(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi)
  (liii pi args)
  (liii pi tools)
  (liii pi tools record)
  (liii pi agent tools)
  (liii pi red-green state)
  (liii pi modes red-green)
) ;import

(check-set-mode! 'report-failed)

;; [046] 验收脚本：red-green TDD 强制
;; 逐条对应 tools/pi/devel/046.md 验收点；纯函数与门禁检查见 046-support.scm，
;; stub server 端到端见 046-support-e2e.scm，本文件负责门禁、行数、13 个新测试
;; 与总调度。

(define %root (path->string (path-cwd)))

(define %tmp '())

(define %impl "tools/pi/devel/tmp-046-impl.py")

(define %test-file "tools/pi/devel/test_tmp_046_gate.py")

(define (%sh tag cmd timeout)
  (let* ((out (string-append "tools/pi/devel/tmp-046-" tag ".out"))
         (err (string-append "tools/pi/devel/tmp-046-" tag ".err"))
        ) ;
    (set! %tmp (append (list out err) %tmp))
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
  (for-each (lambda (f) (when (file-exists? f) (delete-file f)))
    (append (list %impl %test-file) %tmp)
  ) ;for-each
  (set! %tmp '())
) ;define

(load "tools/pi/devel/046-support.scm")
(load "tools/pi/devel/046-support-e2e.scm")

(display "=== 验收点 8: 构建 / fmt / check / doc 门禁 ===\n")
(check (%ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
(check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
(check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)

(display "=== 验收点 8b: 单文件行数 <= 300 ===\n")
(for-each
  (lambda (f)
    (check-true
      (<= (length (string-split (path-read-text f) "\n")) 300)
    ) ;check-true
  ) ;lambda
  '("tools/pi/liii/pi/red-green/state.scm"
    "tools/pi/liii/pi/red-green/classify.scm"
    "tools/pi/liii/pi/red-green/detect.scm"
    "tools/pi/liii/pi/red-green/gate.scm"
    "tools/pi/liii/pi/red-green/observe.scm"
    "tools/pi/liii/pi/red-green/prompt.scm"
    "tools/pi/liii/pi/red-green/report.scm"
    "tools/pi/liii/pi/red-green/tools.scm" "tools/pi/liii/pi/red-green.scm"
    "tools/pi/liii/pi/modes/red-green.scm" "tools/pi/liii/pi/args.scm"
    "tools/pi/liii/pi/cli.scm" "tools/pi/liii/pi/modes.scm"
    "tools/pi/liii/pi/modes/flow.scm" "tools/pi/liii/pi/agent.scm"
    "tools/pi/liii/pi/agent/tools.scm" "tools/pi/liii/pi/tools.scm"
    "tools/pi/liii/pi/prompt.scm" "tools/pi/liii/pi/help.scm"
    "tools/pi/liii/pi.scm" "tools/pi/devel/046.scm"
    "tools/pi/devel/046-support.scm" "tools/pi/devel/046-support-e2e.scm")
) ;for-each

(display "=== 验收点 8c: 13 个新增测试 ===\n")
(for-each
  (lambda (f)
    (check (%ok? "unit" (string-append "timeout 120 bin/gf test " f) 150) => 0)
  ) ;lambda
  '("tools/pi/tests/liii/pi/red-green/state/pi-tdd-advance-phase-test.scm"
    "tools/pi/tests/liii/pi/red-green/state/pi-tdd-add-file-test.scm"
    "tools/pi/tests/liii/pi/red-green/classify/pi-classify-tdd-file-test.scm"
    "tools/pi/tests/liii/pi/red-green/detect/pi-parse-test-run-result-test.scm"
    "tools/pi/tests/liii/pi/red-green/detect/pi-tdd-test-run-command-test.scm"
    "tools/pi/tests/liii/pi/red-green/gate/pi-tdd-write-gate-test.scm"
    "tools/pi/tests/liii/pi/red-green/observe/pi-tdd-observe-tool-test.scm"
    "tools/pi/tests/liii/pi/red-green/prompt/pi-build-red-green-prompt-test.scm"
    "tools/pi/tests/liii/pi/red-green/report/pi-format-tdd-summary-test.scm"
    "tools/pi/tests/liii/pi/red-green/tools/pi-execute-tdd-status-test.scm"
    "tools/pi/tests/liii/pi/agent/pi-execute-one-tool-call-tdd-test.scm"
    "tools/pi/tests/liii/pi/modes/red-green/pi-tdd-extend-tool-names-test.scm"
    "tools/pi/tests/liii/pi/args/pi-parse-pi-args-red-green-test.scm")
) ;for-each

(display "=== 验收点 8d: 既有 CLI / JSON e2e 回归 ===\n")
(for-each
  (lambda (f)
    (check (%ok? "regress" (string-append "timeout 120 bin/gf test " f) 150) => 0)
  ) ;lambda
  '("tools/pi/tests/liii/pi-cli-e2e-test.scm"
    "tools/pi/tests/liii/pi-json-e2e-test.scm")
) ;for-each

(run-cli-args-checks!)
(run-prompt-checks!)
(run-detect-checks!)
(run-gate-checks!)
(run-tools-checks!)
(run-e2e-checks!)

(dynamic-wind (lambda () #f)
  (lambda () (%cleanup!) (display "=== 046 验收完成 ===\n"))
  (lambda () (%cleanup!) (check-report))
) ;dynamic-wind
