(import (liii check)
        (liii gfproject)
        (liii json)
        (liii path))

(check-set-mode! 'report-failed)

;; 1. 测试 gfproject-extract-tools
(check (gfproject-extract-tools '(())) => '(()))
(check (gfproject-extract-tools '(("other" . 123))) => '(()))
(let ((sample '(("tools" . (("doc" . (("module" . "golddoc"))))))))
  (check (json-ref (gfproject-extract-tools sample) "doc" "module") => "golddoc"))

;; 2. 测试 gfproject-deep-merge
;; 空对象合并
(check (gfproject-deep-merge '(( )) '(( ))) => '(( )))
(check (gfproject-deep-merge '((a . 1)) '(( ))) => '((a . 1)))
(check (gfproject-deep-merge '(( )) '((a . 1))) => '((a . 1)))

;; 顶层标量替换与新增键
(let* ((base '(("a" . 1) ("b" . 2)))
       (overlay '(("b" . 20) ("c" . 30)))
       (merged (gfproject-deep-merge base overlay)))
  (check (json-ref merged "a") => 1)
  (check (json-ref merged "b") => 20)
  (check (json-ref merged "c") => 30))

;; 嵌套对象递归合并（保持 base 字段，覆盖 overlay 字段）
(let* ((base '(("tools" . (("test" . (("organization" . "liii")
                                      ("module" . "goldtest")
                                      ("description" . (("en_US" . "Run tests")
                                                        ("zh_CN" . "运行测试")))))))))
       (overlay '(("tools" . (("test" . (("description" . (("zh_CN" . "运行测试（本地覆盖）")))))))))
       (merged (gfproject-deep-merge base overlay))
       (tool (json-ref merged "tools" "test"))
       (desc (json-ref tool "description")))
  (check (json-ref tool "organization") => "liii")
  (check (json-ref tool "module") => "goldtest")
  (check (json-ref desc "en_US") => "Run tests")
  (check (json-ref desc "zh_CN") => "运行测试（本地覆盖）"))

;; 3. 测试 gfproject-load-config 能够读取当前仓库的 gfproject.json
(check (defined? 'g_goldfish-library-dir (rootlet)) => #t)
(check (string? (((rootlet) 'g_goldfish-library-dir))) => #t)
(check (not (not (gfproject-find-lib-path "/home/da/git/goldfish2/goldfish"))) => #t)
(check (not (not (gfproject-find-local-path))) => #t)
(let* ((p (gfproject-find-local-path))
       (txt (path-read-text p)))
  (check (string? txt) => #t))
(let* ((config (gfproject-load-config))
       (tools (gfproject-extract-tools config)))
  (check (json-contains-key? tools "doc") => #t)
  (check (json-contains-key? tools "fmt") => #t)
  (check (json-ref tools "doc" "organization") => "liii")
  (check (json-ref tools "doc" "module") => "golddoc"))

;; 4. 测试 gfproject-resolve-tool 与 gfproject-resolve-tool-bundle
(let ((tool-info (gfproject-resolve-tool "doc")))
  (check (not (not tool-info)) => #t)
  (check (json-ref tool-info "organization") => "liii")
  (check (json-ref tool-info "module") => "golddoc"))

(let ((bundle (gfproject-resolve-tool-bundle "doc")))
  (check (not (not bundle)) => #t)
  (check (json-ref (cdr (assoc "merged-tool" bundle)) "module") => "golddoc"))

(check (gfproject-resolve-tool "non-existent-tool-xyz") => #f)
(check (gfproject-resolve-tool-bundle "non-existent-tool-xyz") => #f)

;; 5. 测试 gfproject-find-tool-root
(let ((root (gfproject-find-tool-root "doc")))
  (check (not (not root)) => #t)
  (check (string? root) => #t))

(check (gfproject-find-tool-root "non-existent-tool-xyz") => #f)

;; 6. 测试动态工具准备与执行测试
(let ((root (gfproject-find-tool-root "version")))
  (check (not (not root)) => #t)
  (set! *load-path* (cons root *load-path*))
  (eval '(import (liii goldversion)) (rootlet))
  (check (procedure? ((rootlet) 'main)) => #t))

(check-report)
