(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi args)
  (liii pi bash)
  (liii pi modes)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; [075] 验收脚本：修复 dispatch-pi-dynamic-test 在无凭据 CI 环境下报错

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-075-" tag ".txt")))
    (let ((ret (run (list (pi-find-bash-path) "-c" (string-append cmd " > " tmp " 2>&1"))
                 :stdin   'null
                 :timeout timeout
               ) ;run
          ) ;ret
         ) ;
      (when (file-exists? tmp)
        (delete-file tmp)
      ) ;when
      ret
    ) ;let
  ) ;let
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)

(display "=== 验收点 2: 文件行数约束 (<= 300 行) ===\n")
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/tests/liii/pi/modes/dispatch-pi-dynamic-test.scm"
    "tools/pi/devel/075.scm")
) ;for-each

(display "=== 验收点 3: 单元测试执行 (dispatch-pi-dynamic-test.scm) ===\n"
) ;display
(check (%run-ok? "unit-test"
         "bin/gf test tools/pi/tests/liii/pi/modes/dispatch-pi-dynamic-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 4: 语义断言（Local Flow 与 Session Flow 在凭据不同条件下的行为） ===\n"
) ;display
;; 4.1 Local flow 无需 api-key 即可运行
(let* ((reg (make-extension-registry))
       (local-hit #f)
       (flow (make-extension-flow-def 'local-f
               (lambda (p) (pi-args-ref p 'my-tag))
               (lambda (p c m i) (set! local-hit #t) 111)
               :local? #t
             ) ;make-extension-flow-def
       ) ;flow
      ) ;
  (register-flag! reg (make-extension-flag-def "--my-tag" "Tag" :key 'my-tag))
  (register-flow! reg flow)
  (with-extension-registry reg
    (lambda ()
      (let ((parsed (parse-pi-args '("--my-tag"))))
        (check (dispatch-pi parsed 'print "" '()) => 111)
        (check-true local-hit)
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

;; 4.2 Session flow 在提供 dummy api-key 时正常执行
(let* ((reg (make-extension-registry))
       (session-hit #f)
       (flow (make-extension-flow-def 'session-f
               (lambda (p) (pi-args-ref p 'sess-tag))
               (lambda (p c m i) (set! session-hit #t) 222)
               :local? #f
             ) ;make-extension-flow-def
       ) ;flow
      ) ;
  (register-flag! reg
    (make-extension-flag-def "--sess-tag" "Session Tag" :key 'sess-tag)
  ) ;register-flag!
  (register-flow! reg flow)
  (with-extension-registry reg
    (lambda ()
      (let ((parsed (parse-pi-args '("--sess-tag" "--api-key" "dummy-token"
                                     "input"))))
        (check (dispatch-pi parsed 'print "input" '()) => 222)
        (check-true session-hit)
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(display "=== [075] 验收脚本执行完成 ===\n")
(check-report)
