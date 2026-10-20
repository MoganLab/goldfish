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

;; [071] 验收脚本：插件目录发现机制与加载器（Discovery & Loader）
;; 逐条对应 tools/pi/devel/071.md 验收点：
;;   1) 门禁检查：构建 / fmt --check / check / doc
;;   2) 代码与测试单文件行数 <= 300 行约束
;;   3) 分层扫描与目录约定发现（Discovery）校验
;;   4) 扩展加载协议与工厂函数调用（Loader）校验
;;   5) 单插件异常隔离与容错（Fault Tolerance）校验
;;   6) 任务 071 专属单元测试套件全绿
;;   7) tools/pi 全量测试套件回归全绿（有限超时 300s）

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-071-" tag ".txt")))
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

(define (get-fixtures-dir)
  (cond ((file-exists? "tests/fixtures/extensions") "tests/fixtures/extensions")
        ((file-exists? "tools/pi/tests/fixtures/extensions")
         "tools/pi/tests/fixtures/extensions"
        ) ;
        (else "../../tools/pi/tests/fixtures/extensions")
  ) ;cond
) ;define

(display "=== 验收点 1: 门禁检查（构建 / fmt / check / doc） ===\n")
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt-chk" "timeout 120 bin/gf fmt --check" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 071 代码与测试文件行数约束 (<= 300 行) ===\n"
) ;display
(let ((files '("tools/pi/liii/pi/extension/discovery.scm"
               "tools/pi/liii/pi/extension/loader.scm"
               "tools/pi/liii/pi/extension.scm"
               "tools/pi/tests/liii/pi/extension/discovery/pi-scan-extension-dir-test.scm"
               "tools/pi/tests/liii/pi/extension/discovery/pi-discover-extensions-test.scm"
               "tools/pi/tests/liii/pi/extension/loader/pi-load-extension-test.scm"
               "tools/pi/tests/liii/pi/extension/loader/pi-load-extensions-test.scm"
               "tools/pi/devel/071.scm")
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

(display "=== 验收点 3: 分层扫描与目录约定发现（Discovery）校验 ===\n"
) ;display
(let* ((fix-dir (get-fixtures-dir))
       (scanned (pi-scan-extension-dir fix-dir))
       (abs-fix-dir (path->string (path-resolve (path fix-dir))))
      ) ;
  ;; 扫描存在文件且按字典序排序
  (check-true (list? scanned))
  (check-true (> (length scanned) 0))

  ;; 4 层级发现测试
  (let ((discovered (pi-discover-extensions
                      :builtin-dir abs-fix-dir
                      :agent-dir   "/non/existent/agent"
                      :cwd         "/non/existent/project"
                      :explicit-extensions
                      (list (string-append abs-fix-dir "/valid-ext1.scm"))
                    ) ;pi-discover-extensions
        ) ;discovered
       ) ;
    (check-true (list? discovered))
    ;; 去重验证：显式指定的 valid-ext1.scm 已在 builtin-dir 中被发现，去重后不增加数量
    (check (length discovered) => (length scanned))
  ) ;let
) ;let*

(display "=== 验收点 4: 扩展加载协议与工厂函数调用（Loader）校验 ===\n"
) ;display
(let* ((api (make-extension-api))
       (fix-dir (get-fixtures-dir))
       (valid-single (string-append fix-dir "/valid-ext1.scm"))
       (valid-pkg (string-append fix-dir "/valid-pkg/index.scm"))
       (res-single (pi-load-extension valid-single api))
       (res-pkg (pi-load-extension valid-pkg api))
      ) ;
  (check (cdr (assq 'status res-single)) => 'ok)
  (check (cdr (assq 'value res-single)) => "ext1-loaded")
  (check (cdr (assq 'status res-pkg)) => 'ok)
  (check (cdr (assq 'value res-pkg)) => "pkg-loaded")

  ;; 注册表验证
  (check-true (extension-tool-def? (find-extension-tool api "test-tool-1")))
  (check-true (extension-flag-def? (find-extension-flag api "--pkg-flag")))
) ;let*

(display "=== 验收点 5: 单插件异常隔离与容错（Fault Tolerance）校验 ===\n"
) ;display
(let* ((api (make-extension-api))
       (fix-dir (get-fixtures-dir))
       (paths (list (string-append fix-dir "/valid-ext1.scm")
                (string-append fix-dir "/syntax-error.scm")
                (string-append fix-dir "/runtime-error.scm")
                (string-append fix-dir "/non-existent.scm")
              ) ;list
       ) ;paths
       (report (pi-load-extensions paths api))
      ) ;
  (check (cdr (assq 'total report)) => 4)
  (check (cdr (assq 'success report)) => 1)
  (check (cdr (assq 'failed report)) => 3)
  (check (pi-load-results-success-count report) => 1)
  (check (pi-load-results-failed-count report) => 3)
  ;; 合法插件不受影响
  (check-true (extension-tool-def? (find-extension-tool api "test-tool-1")))
) ;let*

(display "=== 验收点 6: 任务 071 专属单元测试套件全绿 ===\n")
(let ((ret (%run-ok? "unit-071"
             "timeout 120 bin/gf test tools/pi/tests/liii/pi/extension/discovery/ tools/pi/tests/liii/pi/extension/loader/"
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
