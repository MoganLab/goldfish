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

;; [072] 验收脚本：工具层解耦：接驳动态扩展工具注册表
;; 逐条对应 tools/pi/devel/072.md 验收点：
;;   1) 门禁检查：构建 / fmt --check / check / doc
;;   2) 代码与测试单文件行数 <= 300 行约束
;;   3) 核心解耦与硬编码 import/分支移除校验
;;   4) 内置基础工具兜底与动态扩展工具查找（get-tool-definition）校验
;;   5) 扩展工具动态调用执行、结果包装与异常隔离（execute-tool-call）校验
;;   6) 任务 072 专属单元测试套件全绿
;;   7) tools/pi 全量测试套件回归全绿（有限超时 300s 兜底）

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-072-" tag ".txt")))
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

(display "=== 验收点 2: 任务 072 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(let ((files '("tools/pi/liii/pi/tools.scm"
               "tools/pi/liii/pi/extension/registry.scm"
               "tools/pi/liii/pi/extension.scm" "tools/pi/liii/pi.scm"
               "tools/pi/tests/liii/pi/tools/get-tool-definition-test.scm"
               "tools/pi/tests/liii/pi/tools/execute-tool-call-test.scm"
               "tools/pi/tests/liii/pi/extension/current-extension-registry-test.scm"
               "tools/pi/devel/072.scm")
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

(display "=== 验收点 3: 核心解耦与硬编码 import/分支移除校验 ===\n"
) ;display
(let ((tools-src (path-read-text (path "tools/pi/liii/pi/tools.scm"))))
  ;; 验证 tools.scm 中不再硬编码 import 扩展模块
  (check-false (string-contains tools-src "(liii pi red-green tools)"))
  (check-false (string-contains tools-src "(liii pi blueprint tools)"))
  (check-false (string-contains tools-src "(liii pi compass tools)"))
  ;; 验证 tools.scm 中不再有各扩展工具的硬编码分支
  (check-false (string-contains tools-src "tdd_status"))
  (check-false (string-contains tools-src "tdd_advance"))
  (check-false (string-contains tools-src "tdd_reset"))
  (check-false (string-contains tools-src "blueprint_update"))
  (check-false (string-contains tools-src "blueprint_status"))
  (check-false (string-contains tools-src "blueprint_next"))
  (check-false (string-contains tools-src "codebase_map"))
  (check-false (string-contains tools-src "code_tour"))
) ;let

(display "=== 验收点 4: 内置基础工具兜底与动态扩展工具查找校验 ===\n"
) ;display
;; 8 个内置基础工具在默认状态下正常查找
(let ((builtin-names '("read" "bash" "edit" "write" "ls" "find" "grep"
                       "web_search"))
     ) ;
  (for-each
    (lambda (name)
      (let ((def (get-tool-definition name)))
        (check-true (pi-tool-definition? def))
        (check (pi-tool-definition-name def) => name)
      ) ;let
    ) ;lambda
    builtin-names
  ) ;for-each
) ;let

;; 未注册时，原硬编码工具名返回 #f
(check (get-tool-definition "tdd_status") => #f)
(check (get-tool-definition "blueprint_update") => #f)
(check (get-tool-definition "codebase_map") => #f)

;; 动态注册扩展工具并在注册表作用域内可查找到标准 pi-tool-definition
(let* ((api (make-extension-api))
       (tool-def (make-extension-tool-def "demo_calc"
                   "Demo calculator tool"
                   '((type . "object"))
                   (lambda (args) 100)
                   :snippet    "calc()"
                   :guidelines '("Do not divide by zero")
                 ) ;make-extension-tool-def
       ) ;tool-def
      ) ;
  (register-tool! api tool-def)
  (with-extension-registry api
    (lambda ()
      (let ((found (get-tool-definition "demo_calc")))
        (check-true (pi-tool-definition? found))
        (check (pi-tool-definition-name found) => "demo_calc")
        (check (pi-tool-definition-description found) => "Demo calculator tool")
        (check (pi-tool-definition-snippet found) => "calc()")
        (check (pi-tool-definition-guidelines found) => '("Do not divide by zero"))
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
  ;; 作用域退出后销毁
  (check (get-tool-definition "demo_calc") => #f)
) ;let*

(display "=== 验收点 5: 扩展工具动态调用执行、结果包装与异常隔离校验 ===\n"
) ;display
(let* ((api (make-extension-api))
       (tool-normal (make-extension-tool-def "ping" "Ping tool" '() (lambda (args) "pong"))
       ) ;tool-normal
       (tool-fail (make-extension-tool-def "crash"
                    "Crash tool"
                    '()
                    (lambda (args) (error "intentional-failure"))
                  ) ;make-extension-tool-def
       ) ;tool-fail
      ) ;
  (register-tool! api tool-normal)
  (register-tool! api tool-fail)
  (with-extension-registry api
    (lambda ()
      ;; 正常调用：返回被包裹为 pi-tool-result
      (let ((res (execute-tool-call "ping" '() :timeout 120)))
        (check-true (pi-tool-result? res))
        (check-false (pi-tool-result-is-error res))
        (check
          (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
          =>
          "pong"
        ) ;check
      ) ;let
      ;; 异常隔离：扩展内部抛错被安全包装为 isError: #t，主流程不崩溃
      (let ((res (execute-tool-call "crash" '())))
        (check-true (pi-tool-result? res))
        (check-true (pi-tool-result-is-error res))
        (check
          (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
          =>
          "intentional-failure"
        ) ;check
      ) ;let
    ) ;lambda
  ) ;with-extension-registry
) ;let*

(display "=== 验收点 6: 任务 072 专属单元测试套件全绿 ===\n")
(let ((ret (%run-ok? "unit-072"
             "timeout 120 bin/gf test tools/pi/tests/liii/pi/tools/ tools/pi/tests/liii/pi/extension/current-extension-registry-test.scm"
             120
           ) ;%run-ok?
      ) ;ret
     ) ;
  (check ret => 0)
) ;let

(display "=== 验收点 7: tools/pi 全量测试套件回归全绿 ===\n")
(let ((ret (%run-ok? "all-pi" "timeout 300 bin/gf test tools/pi/tests/liii/pi/" 300))
     ) ;
  (check ret => 0)
) ;let

(check-report)
