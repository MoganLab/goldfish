(import (liii check)
  (liii gfproject)
  (liii json)
  (liii path)
  (liii string)
  (scheme process-context)
) ;import

(check-set-mode! 'report-failed)

;; 1. 测试 gfproject-extract-tools
(check (gfproject-extract-tools '(())) => '(()))
(check (gfproject-extract-tools '(("other" . 123))) => '(()))
(let ((sample '(("tools" ("doc" ("module" . "golddoc"))))))
  (check (json-ref (gfproject-extract-tools sample) "doc" "module") => "golddoc")
) ;let

;; 2. 测试 gfproject-deep-merge
;; 空对象合并
(check (gfproject-deep-merge '(()) '(())) => '(()))
(check (gfproject-deep-merge '((a . 1)) '(())) => '((a . 1)))
(check (gfproject-deep-merge '(()) '((a . 1))) => '((a . 1)))

;; 顶层标量替换与新增键
(let* ((base '(("a" . 1) ("b" . 2)))
       (overlay '(("b" . 20) ("c" . 30)))
       (merged (gfproject-deep-merge base overlay))
      ) ;
  (check (json-ref merged "a") => 1)
  (check (json-ref merged "b") => 20)
  (check (json-ref merged "c") => 30)
) ;let*

;; 嵌套对象递归合并（保持 base 字段，覆盖 overlay 字段）
(let* ((base
         '(("tools"
            ("test"
             ("organization" . "liii")
             ("module" . "goldtest")
             ("description" ("en_US" . "Run tests") ("zh_CN" . "运行测试")))))
       ) ;base
       (overlay
         '(("tools"
            ("test"
             ("description" ("zh_CN" . "运行测试（本地覆盖）")))))
       ) ;overlay
       (merged (gfproject-deep-merge base overlay))
       (tool (json-ref merged "tools" "test"))
       (desc (json-ref tool "description"))
      ) ;
  (check (json-ref tool "organization") => "liii")
  (check (json-ref tool "module") => "goldtest")
  (check (json-ref desc "en_US") => "Run tests")
  (check (json-ref desc "zh_CN") => "运行测试（本地覆盖）")
) ;let*

;; 3. 测试 gfproject-load-config 能够读取当前仓库的 gfproject.json
(check (defined? 'g_goldfish-library-dir (rootlet)) => #t)
(check
  (string? (((rootlet) 'g_goldfish-library-dir)))
  =>
  #t
) ;check
(check (not (not (gfproject-find-lib-path))) => #t)
(check (not (not (gfproject-find-local-path))) => #t)
(let* ((p (gfproject-find-local-path)) (txt (path-read-text p)))
  (check (string? txt) => #t)
) ;let*
(let* ((config (gfproject-load-config)) (tools (gfproject-extract-tools config)))
  (check (json-contains-key? tools "doc") => #t)
  (check (json-contains-key? tools "fmt") => #t)
  (check (json-ref tools "doc" "organization") => "liii")
  (check (json-ref tools "doc" "module") => "golddoc")
) ;let*

;; 4. 测试 gfproject-resolve-tool 与 gfproject-resolve-tool-bundle
(let ((tool-info (gfproject-resolve-tool "doc")))
  (check (not (not tool-info)) => #t)
  (check (json-ref tool-info "organization") => "liii")
  (check (json-ref tool-info "module") => "golddoc")
) ;let

(let ((bundle (gfproject-resolve-tool-bundle "doc")))
  (check (not (not bundle)) => #t)
  (check (json-ref (cdr (assoc "merged-tool" bundle)) "module") => "golddoc")
) ;let

(check (gfproject-resolve-tool "non-existent-tool-xyz") => #f)
(check (gfproject-resolve-tool-bundle "non-existent-tool-xyz") => #f)

;; 5. 测试 gfproject-find-tool-root 与 gfproject-expand-tools-dir
(let ((home (get-environment-variable "HOME")))
  (when (and home (> (string-length home) 0))
    (check (gfproject-expand-tools-dir "~") => home)
    (check (gfproject-expand-tools-dir "~/foo") => (string-append home "/foo"))
  ) ;when
) ;let

(check (gfproject-expand-tools-dir "/abs/path") => "/abs/path")
(let ((rel (gfproject-expand-tools-dir "my_tools")))
  (check (string? rel) => #t)
) ;let

(let ((root (gfproject-find-tool-root "doc")))
  (check (not (not root)) => #t)
  (check (string? root) => #t)
) ;let

(check (gfproject-find-tool-root "non-existent-tool-xyz") => #f)
(check (gfproject-find-tool-root "version" "~/non_existent_tools_dir_xyz")
  =>
  #f
) ;check
(check (gfproject-find-tool-root "doc" #f) => (gfproject-find-tool-root "doc"))

;; 6. 测试动态工具准备与执行测试
(let ((root (gfproject-find-tool-root "version")))
  (check (not (not root)) => #t)
  (set! *load-path* (cons root *load-path*))
  (eval '(import (liii goldversion)) (rootlet))
  (check (procedure? ((rootlet) 'main)) => #t)
) ;let

;; 7. 测试 gfproject-prepare-and-run-tool 的 tool 别名与 tools_dir 禁用回退
;; 7.1 指定 tools_dir 不存在时，即使 allow-fallback 为 #t 也必须严格返回 1（禁用 fallback）
(let* ((gf-lib (gfproject-get-gf-lib))
       (tool-cfg-no-fallback '(("tools_dir" . "~/non_existent_tools_dir_xyz")
                               ("organization" . "liii")
                               ("module" . "goldversion"))
       ) ;tool-cfg-no-fallback
       (err-buf (open-output-string))
       (old-err (set-current-error-port err-buf))
       (res (gfproject-prepare-and-run-tool "version" tool-cfg-no-fallback gf-lib #t))
       (err-msg (get-output-string err-buf))
      ) ;
  (set-current-error-port old-err)
  (check res => 1)
  (check (string-contains? err-msg "directory not found") => #t)
) ;let*

;; 7.2 未指定 tools_dir 且模块错误时，allow-fallback 为 #t 返回 #f（允许 fallback）
(let* ((gf-lib (gfproject-get-gf-lib))
       (tool-cfg-broken '(("organization" . "liii")
                          ("module" . "non_existent_module_xyz"))
       ) ;tool-cfg-broken
       (res (gfproject-prepare-and-run-tool "test-tool" tool-cfg-broken gf-lib #t))
      ) ;
  (check res => #f)
) ;let*

;; 7.3 未指定 tools_dir 且模块错误时，allow-fallback 为 #f 返回 1（直接报错退出）
(let* ((gf-lib (gfproject-get-gf-lib))
       (tool-cfg-broken '(("organization" . "liii")
                          ("module" . "non_existent_module_xyz"))
       ) ;tool-cfg-broken
       (err-buf (open-output-string))
       (old-err (set-current-error-port err-buf))
       (res (gfproject-prepare-and-run-tool "test-tool" tool-cfg-broken gf-lib #f))
       (err-msg (get-output-string err-buf))
      ) ;
  (set-current-error-port old-err)
  (check res => 1)
  (check (string-contains? err-msg "directory not found") => #t)
) ;let*

;; 7.4 测试通过 tool 字段实现别名指向已有工具
(let* ((gf-lib (gfproject-get-gf-lib))
       (alias-root (gfproject-find-tool-root "version" #f gf-lib))
      ) ;
  (check (not (not alias-root)) => #t)
) ;let*

(check-report)
