(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii error)
  (liii string)
  (liii path)
  (liii sys)
  (liii subprocess)
  (liii os)
  (liii pi bash)
  (liii pi extension)
  (liii pi extension discovery)
  (liii pi extension loader)
) ;import

(check-set-mode! 'report-failed)

;; [078] 验收脚本：外部扩展实例示范与端到端实测验证（compass 与 blueprint）

(define (%clean-dir-rec d)
  (let ((p (if (path? d) d (path d))))
    (when (path-dir? p)
      (for-each
        (lambda (e)
          (let ((full (path-join p e)))
            (if (path-dir? full) (%clean-dir-rec full) (delete-file (path->string full)))
          ) ;let
        ) ;lambda
        (vector->list (path-list p))
      ) ;for-each
      (rmdir (path->string p))
    ) ;when
  ) ;let
) ;define

(define (%run-ok? tag cmd timeout)
  (let ((tmp (string-append "tmp-078-" tag ".txt")))
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

(define (%read-stream p stream)
  (let loop
    ((acc ""))
    (let ((c (process-read p :stream stream)))
      (if (eof-object? c) acc (loop (string-append acc c)))
    ) ;let
  ) ;let
) ;define

(define (%run-pi dir args)
  (let* ((p
           (process-spawn (cons (executable) (cons "pi" args))
             :cwd    (path->string (if (path? dir) dir (path dir)))
             :stdout 'pipe
             :stderr 'pipe
           ) ;process-spawn
         ) ;p
         (out (%read-stream p 'stdout))
         (err (%read-stream p 'stderr))
         (code (process-wait p 60000))
        ) ;
    (process-close p)
    (list code out err)
  ) ;let*
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "bin/gf fmt --dry-run" 120) => 0)
(check (%run-ok? "check" "bin/gf check" 120) => 0)
(check (%run-ok? "doc" "bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 078 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(for-each
  (lambda (f)
    (let ((lines (length (string-split (path-read-text (path f)) "\n"))))
      (check-true (<= lines 300))
    ) ;let
  ) ;lambda
  '("tools/pi/liii/pi.scm" "tools/pi/liii/pi/modes.scm"
    "tools/pi/liii/pi/extension/builtin/flags.scm"
    "tools/pi/liii/pi/extension/builtin/flows.scm"
    "tools/pi/liii/pi/extension/builtin/prompts.scm"
    "tools/pi/liii/pi/extension/discovery.scm"
    "tools/pi/liii/pi/extensions/blueprint.scm"
    "tools/pi/tests/liii/pi/extensions/blueprint-test.scm"
    "tools/pi/devel/078.scm")
) ;for-each

(display "=== 验收点 3: 核心解耦与无残留硬编码验证 ===\n")
;; 3.1 扩展文件就位
(check-true (file-exists? "tools/pi/liii/pi/extensions/blueprint.scm"))

;; 3.2 检查 builtin 模块已无 compass/blueprint 残留
(let ((flags-txt (path-read-text (path "tools/pi/liii/pi/extension/builtin/flags.scm"))
      ) ;flags-txt
      (flows-txt (path-read-text (path "tools/pi/liii/pi/extension/builtin/flows.scm"))
      ) ;flows-txt
      (prompts-txt (path-read-text (path "tools/pi/liii/pi/extension/builtin/prompts.scm"))
      ) ;prompts-txt
      (pi-txt (path-read-text (path "tools/pi/liii/pi.scm")))
     ) ;
  (check-false (string-contains? flags-txt "blueprint"))
  (check-false (string-contains? flags-txt "compass"))
  (check-false (string-contains? flows-txt "blueprint"))
  (check-false (string-contains? flows-txt "compass"))
  (check-false (string-contains? prompts-txt "blueprint"))
  (check-false (string-contains? prompts-txt "compass"))
  (check-false (string-contains? pi-txt "(liii pi blueprint)"))
  (check-false (string-contains? pi-txt "(liii pi compass)"))
) ;let

(display "=== 验收点 4: 自动发现与动态加载机制验证 ===\n")
;; 4.1 自动扫描并发现内置扩展
(let ((discovered (pi-discover-extensions)))
  (check-true
    (pair?
      (member (path->string (path-resolve (path "tools/pi/liii/pi/extensions/blueprint.scm")))
        discovered
      ) ;member
    ) ;pair?
  ) ;check-true

  ;; 4.2 加载全部发现的扩展
  (let* ((api (make-extension-api)) (results (pi-load-extensions discovered api)))
    (check-true (pair? results))
    ;; 验证 blueprint 工具与 flags 注册
    (check-true (extension-tool-def? (find-extension-tool api "blueprint_update")))
    (check-true (extension-tool-def? (find-extension-tool api "blueprint_status")))
    (check-true (extension-tool-def? (find-extension-tool api "blueprint_next")))
    (check-true (extension-flag-def? (find-extension-flag api "--blueprint")))
    (check-true (extension-flag-def? (find-extension-flag api "--plan-status")))
    (check-true (extension-flag-def? (find-extension-flag api "--plan-next")))
  ) ;let*
) ;let

(display "=== 验收点 5: 功能完整性端到端 CLI 实测 ===\n")
(let* ((work-dir (path-join (path-temp-dir) "gf-tmp-078-e2e-work"))
       (work-str (path->string work-dir))
      ) ;
  (when (path-exists? work-dir)
    (%clean-dir-rec work-dir)
  ) ;when
  (path-mkdir work-dir :parents #t :exist-ok #t)

  ;; 在工作目录建立简单测试文件供 codemap 分析
  (path-mkdir (path-join work-dir "src") :parents #t :exist-ok #t)
  (path-write-text (path-join work-dir "src" "main.scm") "(define x 1)\n")

  ;; 5.1 在隔离工作目录下运行 --plan-status
  (let ((res (%run-pi work-dir '("--print" "--plan-status"))))
    (check (car res) => 0)
    (check-true (string-contains? (cadr res) "No active blueprint."))
  ) ;let

  ;; 清理隔离目录
  (%clean-dir-rec work-dir)
  (check-false (file-exists? work-str))
) ;let*

;; 5.4 严格确保本工程根目录下未残留 .pi
(check-false (file-exists? ".pi"))

(display "=== 验收点 6: 全量单元测试回归 ===\n")
(check (%run-ok? "all-tests" "bin/gf test tools/pi/tests/liii/" 300) => 0)

(check-report)
