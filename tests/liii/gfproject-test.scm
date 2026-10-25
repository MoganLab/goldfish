(import (liii check)
  (liii gfproject tools)
  (liii json)
  (liii os)
  (liii path)
  (liii string)
  (liii tempfile)
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
    (check (gfproject-expand-tools-dir "~/foo")
      =>
      (path->string (path-join home "foo"))
    ) ;check
  ) ;when
) ;let

(when (not (os-windows?))
  (check (gfproject-expand-tools-dir "/abs/path") => "/abs/path")
) ;when
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

;; 8. 测试多级子库路径切分与 import-spec 构造
(check (gfproject-parse-import-spec "liii" "fmt") => '(liii fmt))
(check (gfproject-parse-import-spec "liii" "pi/cli") => '(liii pi cli))
(check (gfproject-parse-import-spec "liii" "/pi/cli/") => '(liii pi cli))
(check (gfproject-parse-import-spec "liii" "a/b/c") => '(liii a b c))
(check (gfproject-parse-import-spec "liii" "///pi///cli///") => '(liii pi cli))

;; 8.1 测试多级模块在 allow-fallback 为 #f 时的精准报错格式
(let* ((gf-lib (gfproject-get-gf-lib))
       (tool-cfg-multi '(("organization" . "liii")
                         ("module" . "pi/non_existent_cli_xyz"))
       ) ;tool-cfg-multi
       (err-buf (open-output-string))
       (old-err (set-current-error-port err-buf))
       (res (gfproject-prepare-and-run-tool "version" tool-cfg-multi gf-lib #f))
       (err-msg (get-output-string err-buf))
      ) ;
  (set-current-error-port old-err)
  (check res => 1)
  (check (string-contains? err-msg "Error importing (liii pi non_existent_cli_xyz)")
    =>
    #t
  ) ;check
  ;; [1128] 错误信息应完整格式化：~S 占位符被替换，并指明缺失的文件路径
  (check (string-contains? err-msg "cannot find file") => #t)
  (check (string-contains? err-msg "liii/pi/non_existent_cli_xyz.scm") => #t)
) ;let*

;; 9. [1128] 错误信息格式化：s7 的 error info 是 (格式串 参数...) 列表，需完整格式化后呈现

(check (gfproject-error->string '("value is ~S" 42)) => "value is 42")
(check (gfproject-error->string '("lib ~S missing" (no such lib))) => "lib (no such lib) missing")
;; 非列表的 info：原样转换为字符串
(check (gfproject-error->string 42) => "42")
(check (gfproject-error->string "plain") => "plain")
;; 首元素非格式串的列表：回退到整体打印
(check (gfproject-error->string '(a b)) => "(a b)")

;; 10. [1131] 内置工具隔离：本地 gfproject.json 与内置重名时必须提示冲突，
;;    且 cwd 下的 tools/<内置名> 不得遮蔽内置插件（cwd 仅作自定义工具的兜底）
;; 10.1 gfproject-tool-conflict? 纯函数
;; 同源（lib_path 与 local_path 归一化后为同一文件）=> 不冲突（bin/gf 开发场景）
(check
  (gfproject-tool-conflict? "doc"
    '(("lib_path" . "/repo/goldfish/../gfproject.json")
      ("local_path" . "/repo/gfproject.json")
      ("lib_tools" ("doc" ("module" . "golddoc")))
      ("local_tools" ("doc" ("module" . "golddoc"))))
    )
  => #f
) ;check

;; 不同源且两边重名 => 冲突
(check
  (gfproject-tool-conflict? "doc"
    '(("lib_path" . "/usr/share/goldfish/gfproject.json")
      ("local_path" . "/proj/gfproject.json")
      ("lib_tools" ("doc" ("module" . "golddoc")))
      ("local_tools" ("doc" ("module" . "mydoc"))))
    )
  => #t
) ;check

;; 不同源但仅本地注册 => 不冲突（自定义项目工具）
(check
  (gfproject-tool-conflict? "mytool"
    '(("lib_path" . "/usr/share/goldfish/gfproject.json")
      ("local_path" . "/proj/gfproject.json")
      ("lib_tools" ("doc" ("module" . "golddoc")))
      ("local_tools" ("mytool" ("module" . "mytool"))))
    )
  => #f
) ;check

;; lib_path 缺失（无内置注册表）=> 不冲突
(check
  (gfproject-tool-conflict? "doc"
    '(("lib_path" . #f)
      ("local_path" . "/proj/gfproject.json")
      ("lib_tools" . (()))
      ("local_tools" ("doc" ("module" . "mydoc"))))
    )
  => #f
) ;check

;; 10.2 真实仓库：lib_path 与 local_path 同源 => doc 不冲突
(let ((bundle (gfproject-resolve-tool-bundle "doc")))
  (check (string? (cdr (assoc "lib_path" bundle))) => #t)
  (check (gfproject-tool-conflict? "doc" bundle) => #f)
) ;let

;; 10.3 gfproject-find-tool-root：内置目录优先于 cwd 同名目录，cwd 仅作兜底
;; 注：路径统一用 chdir 后 getcwd 的真实形态，避免 macOS $TMPDIR 符号链接
;; （/var -> /private/var）造成的期望值/实际值前缀差异
(with-temp-dir
  (lambda (tmp)
    (let ((old-cwd (getcwd)))
      (dynamic-wind
        (lambda () (chdir tmp))
        (lambda ()
          (let* ((real-tmp (getcwd))
                 (fake-lib (path-join real-tmp "fake-lib"))
                 (proj (path-join real-tmp "proj"))
                ) ;
            (path-mkdir (path-join fake-lib "tools" "doc") :parents #t)
            (path-mkdir (path-join proj "tools" "doc") :parents #t)
            (dynamic-wind
              (lambda () (chdir (path->string proj)))
              (lambda ()
                ;; 内置侧存在时，即使 cwd 也有同名目录，也必须解析到内置侧
                (check (gfproject-find-tool-root "doc" #f (path->string fake-lib))
                  =>
                  (path->string (path-join fake-lib "tools" "doc"))
                ) ;check
                ;; 内置侧不存在时，cwd 兜底仍然可用（自定义工具场景）
                (path-rmdir (path-join fake-lib "tools" "doc"))
                (check (gfproject-find-tool-root "doc" #f (path->string fake-lib))
                  =>
                  (path->string (path-join proj "tools" "doc"))
                ) ;check
              ) ;lambda
              (lambda () (chdir real-tmp))
            ) ;dynamic-wind
          ) ;let*
        ) ;lambda
        (lambda () (chdir old-cwd))
      ) ;dynamic-wind
    ) ;let
  ) ;lambda
) ;with-temp-dir

;; 10.4 集成：本地 gfproject.json 与内置重名 => 提示忽略本地注册，内置实现正常执行
(with-temp-dir
  (lambda (tmp)
    (let ((old-cwd (getcwd)))
      (path-write-text (path-join tmp "gfproject.json")
        "{\"tools\": {\"version\": {\"organization\": \"liii\", \"module\": \"goldversion\"}}}")
      (dynamic-wind
        (lambda () (chdir tmp))
        (lambda ()
          (let* ((err-buf (open-output-string))
                 (old-err (set-current-error-port err-buf))
                 (res (gfproject-run-tool "version" (gfproject-get-gf-lib)))
                 (err-msg (get-output-string err-buf))
                ) ;
            (set-current-error-port old-err)
            ;; Warning 直写进程 stderr（不经过捕获 port，由 load-gfproject 的
            ;; shell 级测试断言），这里只断言内置实现正常执行
            (check res => 0)
            (check (string-contains? err-msg "ignoring local registration") => #f)
          ) ;let*
        ) ;lambda
        (lambda () (chdir old-cwd))
      ) ;dynamic-wind
    ) ;let
  ) ;lambda
) ;with-temp-dir

(check-report)
