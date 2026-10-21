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
  (liii pi bash)
  (liii pi modes)
  (liii pi extension)
) ;import

(check-set-mode! 'report-failed)

;; [076] 验收脚本：内置扩展收敛迁移（simplify, red-green, code-review）

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-076-" tag ".txt")))
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

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 076 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi/extensions/simplify.scm"
    "tools/pi/liii/pi/extensions/red-green.scm"
    "tools/pi/liii/pi/extensions/code-review.scm"
    "tools/pi/liii/pi/args/query.scm" "tools/pi/liii/pi/extension/discovery.scm"
    "tools/pi/liii/pi/extension/builtin/flags.scm"
    "tools/pi/liii/pi/extension/builtin/flows.scm"
    "tools/pi/liii/pi/extension/builtin/prompts.scm"
    "tools/pi/liii/pi/extension/builtin/tdd.scm"
    "tools/pi/tests/liii/pi/extensions/simplify-test.scm"
    "tools/pi/tests/liii/pi/extensions/red-green-test.scm"
    "tools/pi/tests/liii/pi/extensions/code-review-test.scm"
    "tools/pi/devel/076.scm")
) ;for-each

(display "=== 验收点 3: 核心层彻底解耦，移除内置硬编码残留 ===\n"
) ;display
;; 验证 flags.scm 与 flows.scm 中已无 simplify, red-green, code-review
(let ((flags-src (path-read-text (path "tools/pi/liii/pi/extension/builtin/flags.scm"))
      ) ;flags-src
      (flows-src (path-read-text (path "tools/pi/liii/pi/extension/builtin/flows.scm"))
      ) ;flows-src
      (prompts-src (path-read-text (path "tools/pi/liii/pi/extension/builtin/prompts.scm"))
      ) ;prompts-src
      (tdd-src (path-read-text (path "tools/pi/liii/pi/extension/builtin/tdd.scm")))
      (args-src (path-read-text (path "tools/pi/liii/pi/args.scm")))
     ) ;
  (check-false (string-contains flags-src "--red-green"))
  (check-false (string-contains flags-src "--simplify"))
  (check-false (string-contains flags-src "--code-review"))
  (check-false (string-contains flows-src "simplify"))
  (check-false (string-contains flows-src "code-review"))
  (check-false (string-contains prompts-src "code-review"))
  (check-false (string-contains tdd-src "pi-tdd-write-gate"))
  (check-false (string-contains args-src "--red-green"))
  (check-false (string-contains args-src "--simplify"))
  (check-false (string-contains args-src "--code-review"))
) ;let

(display "=== 验收点 4: 扩展目录收敛与命令注册校验 ===\n")
;; 4.1 测试 red-green 注册 /tdd, /tdd-status, --red-green
(let* ((api (make-extension-api))
       (res (pi-load-extension "tools/pi/liii/pi/extensions/red-green.scm" api))
      ) ;
  (check (cdr (assq 'status res)) => 'ok)
  (check-true (extension-flag-def? (find-extension-flag api "--red-green")))
  (check-true (extension-command-def? (find-extension-command api "/tdd")))
  (check-true (extension-command-def? (find-extension-command api "tdd")))
  (check-true (extension-command-def? (find-extension-command api "/tdd-status")))
  (check-true (extension-command-def? (find-extension-command api "tdd-status")))
  (check-true (extension-tool-def? (find-extension-tool api "tdd_status")))
  (check-true (extension-tool-def? (find-extension-tool api "tdd_advance")))
  (check-true (extension-tool-def? (find-extension-tool api "tdd_reset")))
) ;let*

;; 4.2 测试 simplify 注册 --simplify, --apply, /simplify
(let* ((api (make-extension-api))
       (res (pi-load-extension "tools/pi/liii/pi/extensions/simplify.scm" api))
      ) ;
  (check (cdr (assq 'status res)) => 'ok)
  (check-true (extension-flag-def? (find-extension-flag api "--simplify")))
  (check-true (extension-flag-def? (find-extension-flag api "--apply")))
  (check-true (extension-command-def? (find-extension-command api "/simplify")))
  (check-true (extension-command-def? (find-extension-command api "simplify")))
) ;let*

;; 4.3 测试 code-review 注册 --code-review, /review, prompt injector
(let* ((api (make-extension-api))
       (res (pi-load-extension "tools/pi/liii/pi/extensions/code-review.scm" api))
      ) ;
  (check (cdr (assq 'status res)) => 'ok)
  (check-true (extension-flag-def? (find-extension-flag api "--code-review")))
  (check-true (extension-flag-def? (find-extension-flag api "--auto-review")))
  (check-true (extension-flag-def? (find-extension-flag api "--staged")))
  (check-true (extension-flag-def? (find-extension-flag api "--ref")))
  (check-true (extension-command-def? (find-extension-command api "/review")))
  (check-true (extension-command-def? (find-extension-command api "review")))
  (check-true (extension-command-def? (find-extension-command api "/code-review"))
  ) ;check-true
  (check-true (extension-command-def? (find-extension-command api "code-review")))
  (check-true (pair? (extension-registry-prompt-injectors (extension-api-registry api)))
  ) ;check-true
) ;let*

(display "=== 验收点 5: 默认内置扩展目录扫描加载完整运作 ===\n"
) ;display
(let* ((builtin-dir (pi-default-builtin-extensions-dir))
       (ext-paths (pi-discover-extensions
                    :builtin-dir   builtin-dir
                    :load-global?  #f
                    :load-project? #f
                  ) ;pi-discover-extensions
       ) ;ext-paths
       (file-names
         (map (lambda (p) (path-name (path p))) ext-paths)
       ) ;file-names
      ) ;
  (check-true (file-exists? builtin-dir))
  (check-true (and (member "red-green.scm" file-names) #t))
  (check-true (and (member "simplify.scm" file-names) #t))
  (check-true (and (member "code-review.scm" file-names) #t))
) ;let*

(display "=== 验收点 6: 任务 076 专属单元测试套件全绿 ===\n")
(check (%run-ok? "ext-tests" "bin/gf test tools/pi/tests/liii/pi/extensions/" 120)
  =>
  0
) ;check

(display "=== 验收点 7: tools/pi 全量测试套件回归全绿 ===\n")
(check (%run-ok? "all-tests" "bin/gf test tools/pi/tests/liii/" 300) => 0)

(display "=== [076] 验收脚本执行完成 ===\n")
(check-report)
