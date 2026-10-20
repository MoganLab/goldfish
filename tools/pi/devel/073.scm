(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi)
  (liii pi args)
  (liii pi modes)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; [073] 验收脚本：命令行与路由解耦：动态 Flags 挂载与 Local Flow 分发
;; 逐条对应 tools/pi/devel/073.md 验收点：
;;   1) 门禁检查：构建 / fmt --check / check / doc
;;   2) 代码与测试单文件行数 <= 300 行约束
;;   3) 核心解耦与硬编码 import / 分支移除校验
;;   4) 动态 Flags 挂载、别名、--opt=val 与默认值解析校验
;;   5) 动态 Local Flow 与 Session Flow 路由分发校验
;;   6) 动态 Validator 验证机制校验
;;   7) 核心测试套件回归全绿（args, modes, extension, compass 全量通过）

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-073-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text (path tmp-out))
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

(define (%run-ok? tag cmd timeout)
  (car (%run-capture tag cmd timeout))
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "timeout 120 bin/gf fmt --check" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 073 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(let ((files '("tools/pi/liii/pi.scm" "tools/pi/liii/pi/tools.scm"
               "tools/pi/liii/pi/args.scm" "tools/pi/liii/pi/modes.scm"
               "tools/pi/liii/pi/cli.scm" "tools/pi/liii/pi/extension.scm"
               "tools/pi/liii/pi/extension/types.scm"
               "tools/pi/liii/pi/extension/types/core.scm"
               "tools/pi/liii/pi/extension/types/defs.scm"
               "tools/pi/liii/pi/extension/registry.scm"
               "tools/pi/liii/pi/extension/registry/base.scm"
               "tools/pi/liii/pi/extension/registry/flags.scm"
               "tools/pi/liii/pi/extension/registry/flows.scm"
               "tools/pi/liii/pi/extension/registry/commands.scm"
               "tools/pi/liii/pi/extension/registry/tools.scm"
               "tools/pi/liii/pi/extension/builtin.scm"
               "tools/pi/liii/pi/extension/builtin/flags.scm"
               "tools/pi/liii/pi/extension/builtin/flows.scm"
               "tools/pi/tests/liii/pi/args/parse-pi-args-dynamic-test.scm"
               "tools/pi/tests/liii/pi/modes/dispatch-pi-dynamic-test.scm"
               "tools/pi/tests/liii/pi/extension/registry/register-flag-dynamic-test.scm"
               "tools/pi/tests/liii/pi/extension/registry/register-flow-dynamic-test.scm"
               "tools/pi/devel/073.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (let ((line-count (length (string-split (path-read-text (path f)) "\n"))))
        (check-true (<= line-count 300))
      ) ;let
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 3: 核心解耦与硬编码 import / 分支移除校验 ===\n"
) ;display
(let ((args-src (path-read-text (path "tools/pi/liii/pi/args.scm")))
      (modes-src (path-read-text (path "tools/pi/liii/pi/modes.scm")))
      (cli-src (path-read-text (path "tools/pi/liii/pi/cli.scm")))
     ) ;
  ;; args.scm / modes.scm / cli.scm 不再 import blueprint 与 compass 的 args 模块
  (check-false (string-contains args-src "(liii pi blueprint args)"))
  (check-false (string-contains args-src "(liii pi compass args)"))
  (check-false (string-contains modes-src "(liii pi blueprint args)"))
  (check-false (string-contains modes-src "(liii pi compass args)"))
  (check-false (string-contains cli-src "(liii pi blueprint args)"))
  (check-false (string-contains cli-src "(liii pi compass args)"))

  ;; modes.scm 不再硬编码各扩展 local flow / session flow 分支
  (check-false (string-contains modes-src "pi-compass-local-flow?"))
  (check-false (string-contains modes-src "pi-blueprint-local-flow?"))
  (check-false (string-contains modes-src "pi-blueprint-flag?"))

  ;; cli.scm 不再硬编码各扩展标志的特定文案检查
  (check-false (string-contains cli-src "--red-green requires a task description")
  ) ;check-false
  (check-false (string-contains cli-src "--blueprint requires an objective"))
) ;let

(display "=== 验收点 4: 动态 Flags 挂载与解析校验 ===\n")
(let ((reg (make-extension-registry)))
  (register-flag! reg
    (make-extension-flag-def "--custom-debug" "Custom debug" :key 'custom-debug)
  ) ;register-flag!
  (register-flag! reg
    (make-extension-flag-def "--engine"
      "Engine type"
      :key        'engine
      :aliases    '("-e")
      :takes-arg? #t
      :default    "default-eng"
    ) ;make-extension-flag-def
  ) ;register-flag!
  (with-extension-registry reg
    (lambda ()
      ;; 默认值注入
      (let ((p0 (parse-pi-args '())))
        (check (pi-args-ref p0 'custom-debug) => #f)
        (check (pi-args-ref p0 'engine) => "default-eng")
      ) ;let
      ;; 布尔选项解析
      (let ((p1 (parse-pi-args '("--custom-debug"))))
        (check (pi-args-ref p1 'custom-debug) => #t)
      ) ;let
      ;; 带参选项空格传参与别名
      (let ((p2 (parse-pi-args '("-e" "turbo"))))
        (check (pi-args-ref p2 'engine) => "turbo")
      ) ;let
      ;; 带参选项等号传参
      (let ((p3 (parse-pi-args '("--engine=v8"))))
        (check (pi-args-ref p3 'engine) => "v8")
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let

(display "=== 验收点 5: 动态 Local Flow 与 Session Flow 路由分发校验 ===\n"
) ;display
(let* ((reg (make-extension-registry))
       (local-hit #f)
       (session-hit #f)
       (flow-local (make-extension-flow-def 'test-local
                     (lambda (p) (pi-args-ref p 'flow-local-flag))
                     (lambda (p c m msg) (set! local-hit #t) 77)
                     :local? #t
                   ) ;make-extension-flow-def
       ) ;flow-local
       (flow-session (make-extension-flow-def 'test-session
                       (lambda (p) (pi-args-ref p 'flow-session-flag))
                       (lambda (p c m msg) (set! session-hit #t) 88)
                       :local? #f
                     ) ;make-extension-flow-def
       ) ;flow-session
      ) ;
  (register-flag! reg
    (make-extension-flag-def "--flow-local-flag" "Local" :key 'flow-local-flag)
  ) ;register-flag!
  (register-flag! reg
    (make-extension-flag-def "--flow-session-flag"
      "Session"
      :key 'flow-session-flag
    ) ;make-extension-flag-def
  ) ;register-flag!
  (register-flow! reg flow-local)
  (register-flow! reg flow-session)
  (with-extension-registry reg
    (lambda ()
      ;; 1) Local flow 无凭据优先命中执行
      (let ((p-local (parse-pi-args '("--flow-local-flag"))))
        (check (dispatch-pi p-local 'print "" '()) => 77)
        (check local-hit => #t)
      ) ;let
      ;; 2) Session flow 在有配置时命中执行
      (let ((p-session (parse-pi-args '("--flow-session-flag" "hello"))))
        (check (dispatch-pi p-session 'print "hello" '()) => 88)
        (check session-hit => #t)
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(display "=== 验收点 6: 动态 Validator 校验机制 ===\n")
(let ((reg (make-extension-registry)))
  (register-flag! reg (make-extension-flag-def "--f-a" "A" :key 'f-a))
  (register-flag! reg (make-extension-flag-def "--f-b" "B" :key 'f-b))
  (register-validator! reg
    (lambda (parsed . extra)
      (when (and (pi-args-ref parsed 'f-a) (pi-args-ref parsed 'f-b))
        (error 'pi-arg-error "--f-a and --f-b conflict")
      ) ;when
    ) ;lambda
  ) ;register-validator!
  (with-extension-registry reg
    (lambda ()
      (check (pi-validate-review-flags (parse-pi-args '("--f-a"))) => #t)
      (check-catch 'pi-arg-error
        (pi-validate-review-flags (parse-pi-args '("--f-a" "--f-b")))
      ) ;check-catch
    ) ;lambda
  ) ;with-extension-registry
) ;let

(display "=== 验收点 7: 全套相关单元测试与回归套件执行 ===\n")
(check (%run-ok? "test-args"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/args/"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-modes"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/modes/"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-ext"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/extension/"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-compass"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/compass/args/"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-bp-e2e"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/modes/blueprint/pi-blueprint-cli-e2e-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-cli-e2e"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi-cli-e2e-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "test-all-pi" "timeout 300 bin/gf test tools/pi/tests/liii/pi/" 300)
  =>
  0
) ;check

(check-report)
