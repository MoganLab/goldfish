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
) ;import

(check-set-mode! 'report-failed)

;; [070] 验收脚本：Extension 核心抽象与 ExtensionAPI
;; 逐条对应 tools/pi/devel/070.md 验收点：
;;   1) 门禁检查：构建 / fmt --check / check / doc
;;   2) 代码与测试单文件行数 <= 300 行约束
;;   3) 核心 Record 类型定义与契约校验
;;   4) 工具/标志/命令注册与查找功能校验
;;   5) 事件生命周期派发、拦截与异常隔离校验
;;   6) 任务 070 专属单元测试套件全绿
;;   7) tools/pi 全量测试套件回归全绿（有限超时 300s）

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-070-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
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

(display "=== 验收点 2: 任务 070 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(let ((files '("tools/pi/liii/pi/extension.scm"
               "tools/pi/liii/pi/extension/types.scm"
               "tools/pi/liii/pi/extension/registry.scm"
               "tools/pi/liii/pi/extension/event.scm"
               "tools/pi/tests/liii/pi/extension/make-extension-api-test.scm"
               "tools/pi/tests/liii/pi/extension/make-extension-context-test.scm"
               "tools/pi/tests/liii/pi/extension/pi-emit-test.scm"
               "tools/pi/tests/liii/pi/extension/pi-on-test.scm"
               "tools/pi/tests/liii/pi/extension/register-command-test.scm"
               "tools/pi/tests/liii/pi/extension/register-flag-test.scm"
               "tools/pi/tests/liii/pi/extension/register-tool-test.scm"
               "tools/pi/devel/070.scm")
      ) ;files
     ) ;
  (for-each
    (lambda (f)
      (let ((line-count (length (string-split (path-read-text f) "\n"))))
        (check-true (<= line-count 300))
      ) ;let
    ) ;lambda
    files
  ) ;for-each
) ;let

(display "=== 验收点 3: 核心 Record 类型定义与参数校验 ===\n")
(let ((reg (make-extension-registry))
      (api (make-extension-api))
      (ctx (make-extension-context :session-id "s-1" :cwd "/test" :model "kimi"))
      (tool (make-extension-tool-def "t1" "desc" '() (lambda (args) "ok")))
      (flag (make-extension-flag-def "--my-flag" "flag desc"))
      (cmd (make-extension-command-def "/do" "cmd desc" (lambda (arg) arg)))
     ) ;
  (check-true (extension-registry? reg))
  (check-true (extension-api? api))
  (check-true (extension-context? ctx))
  (check (extension-context-session-id ctx) => "s-1")
  (check (extension-context-cwd ctx) => "/test")
  (check (extension-context-model ctx) => "kimi")
  (check-true (extension-tool-def? tool))
  (check (extension-tool-def-name tool) => "t1")
  (check-true (extension-flag-def? flag))
  (check (extension-flag-def-flag flag) => "--my-flag")
  (check-true (extension-command-def? cmd))
  (check (extension-command-def-name cmd) => "/do")
  ;; 参数校验
  (check-catch 'type-error (make-extension-tool-def "" "d" '() (lambda () #f)))
  (check-catch 'type-error (make-extension-flag-def "" "d"))
  (check-catch 'type-error (make-extension-command-def "" "d" (lambda () #f)))
  (check-catch 'type-error (make-extension-context :cwd 123))
) ;let

(display "=== 验收点 4: 扩展注册表功能校验 (Tool, Flag, Command) ===\n"
) ;display
(let* ((api (make-extension-api))
       (tool-res
         (extension-register-tool! api
           (list (cons 'name "custom_tool")
             (cons 'description "custom tool desc")
             (cons 'parameters '())
             (cons 'execute (lambda (args) "result-ok"))
           ) ;list
         ) ;extension-register-tool!
       ) ;tool-res
       (flag-res (extension-register-flag! api
                   '((flag . "--custom-opt")
                     (description . "custom opt desc")
                     (type . string)
                     (default . "val"))
                 ) ;extension-register-flag!
       ) ;flag-res
       (cmd-res
         (extension-register-command! api
           (list (cons 'name "/custom-cmd")
             (cons 'description "custom cmd desc")
             (cons 'handler (lambda (args) "cmd-ok"))
           ) ;list
         ) ;extension-register-command!
       ) ;cmd-res
      ) ;
  (check-true (extension-tool-def? tool-res))
  (check (extension-tool-def-name (find-extension-tool api "custom_tool"))
    =>
    "custom_tool"
  ) ;check
  (check (length (extension-registry-tools-list api)) => 1)

  (check-true (extension-flag-def? flag-res))
  (check (extension-flag-def-flag (find-extension-flag api "--custom-opt"))
    =>
    "--custom-opt"
  ) ;check
  (check (length (extension-registry-flags-list api)) => 1)

  (check-true (extension-command-def? cmd-res))
  (check (extension-command-def-name (find-extension-command api "/custom-cmd"))
    =>
    "/custom-cmd"
  ) ;check
  (check (length (extension-registry-commands-list api)) => 1)
) ;let*

(display "=== 验收点 5: 事件生命周期派发、拦截与异常隔离 ===\n"
) ;display
;; 1) 正常事件流派发
(let ((api (make-extension-api)) (events-seen '()))
  (for-each
    (lambda (evt)
      (pi-on! api
        evt
        (lambda (payload ctx) (set! events-seen (cons evt events-seen)))
      ) ;pi-on!
    ) ;lambda
    '(session_start agent_start tool_result agent_end)
  ) ;for-each
  (pi-emit! api 'session_start '((id . "1")))
  (pi-emit! api 'agent_start '((prompt . "hello")))
  (pi-emit! api 'tool_result '((result . "done")))
  (pi-emit! api 'agent_end '((status . "settled")))
  (check (reverse events-seen)
    =>
    '(session_start agent_start tool_result agent_end)
  ) ;check
) ;let

;; 2) tool_call 拦截 (block)
(let ((api (make-extension-api)))
  (pi-on! api
    'tool_call
    (lambda (payload) '(block . "Operation forbidden by security extension"))
  ) ;pi-on!
  (let ((res (pi-emit! api 'tool_call '((name . "bash") (input . "rm -rf /")))))
    (check (cdr (assq 'action res)) => 'block)
    (check (cdr (assq 'reason res)) => "Operation forbidden by security extension")
  ) ;let
) ;let

;; 3) tool_call 输入改写 (override)
(let ((api (make-extension-api)))
  (pi-on! api
    'tool_call
    (lambda (payload) (list 'override '((path . "safe.scm"))))
  ) ;pi-on!
  (let ((res
          (pi-emit! api 'tool_call '((name . "read")
                                     (input (path . "secret.scm"))))
        ) ;res
       ) ;
    (check (cdr (assq 'action res)) => 'continue)
    (check
      (cdr (assq 'input (cdr (assq 'payload res))))
      =>
      '((path . "safe.scm"))
    ) ;check
  ) ;let
) ;let

;; 4) 监听器异常隔离：报错不中断后续监听器
(let ((api (make-extension-api)) (step2-run #f))
  (pi-on! api 'tool_call (lambda (payload) (type-error "intentional failure")))
  (pi-on! api 'tool_call (lambda (payload) (set! step2-run #t)))
  (let ((res (pi-emit! api 'tool_call '((name . "test")))))
    (check (cdr (assq 'action res)) => 'continue)
    (check-true step2-run)
  ) ;let
) ;let

(display "=== 验收点 6: 任务 070 单元测试套件全绿 ===\n")
(check (%run-ok? "ext-tests"
         "timeout 120 bin/gf test tools/pi/tests/liii/pi/extension/"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 7: tools/pi 全量测试套件回归全绿 (有限超时 300s) ===\n"
) ;display
(check (%run-ok? "all-tests" "timeout 300 bin/gf test tools/pi/tests/liii/pi/" 300)
  =>
  0
) ;check

(check-report)
