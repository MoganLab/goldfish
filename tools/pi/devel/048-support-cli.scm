;; [048] 验收脚本辅助函数（续）：tour / Scheme 适配 / 注入 / 工具 / 门禁。

(define (%check-tour-cli!)
  (display "=== 验收点 4: code tour ===\n")
  (let ((r (%run-pi %work "--print" "--tour")))
    (check (car r) => 0)
    (for-each
      (lambda (s) (check-true (string-contains? (cadr r) s)))
      '("Available tour topics:" "  - src" "  - testing" "  - ci")
    ) ;for-each
  ) ;let
  (let ((r (%run-pi %work "--print" "--tour" "src")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "## Code Tour: src"))
    (check-true (string-contains? (cadr r) "`src/index.js`"))
    (check-true (string-contains? (cadr r) "Entry point for src module"))
  ) ;let
  (let ((r (%run-pi %work "--print" "--tour" "testing")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "Test file for login"))
  ) ;let
  (let ((r (%run-pi %work "--print" "--tour" "no-such-topic")))
    (check (car r) => 0)
    (check (string-trim-both (cadr r))
      =>
      "No files found for topic \"no-such-topic\"."
    ) ;check
  ) ;let
  ;; tour 不落盘：.pi 下仍只有 codemap.json。
  (check
    (length
      (vector->list (path-list (path (string-append %work "/.pi"))))
    ) ;length
    =>
    1
  ) ;check
) ;define

(define (%check-scheme-fixture!)
  (display "=== 验收点 2b/4b: Scheme 工程适配 ===\n")
  (%copy-dir! %scheme-fixture %work-scheme)
  ;; 临时副本移除 tools/pi：否则 bin/gf 会把 cwd/tools/pi 当作 load-path，
  ;; 遮蔽 (liii pi cli) 的真实库位置（与 compass 行为无关）。
  (let ((r (%sh "rm-tools" (string-append "rm -rf '" %work-scheme "/tools'") 60)))
    (check (car r) => 0)
  ) ;let
  (let ((r (%run-pi %work-scheme "--print" "--codemap")))
    (check (car r) => 0)
    (check-false (string-contains? (cadr r) "### Packages"))
    (check-true (string-contains? (cadr r) "### Entry Points"))
    (check-true (string-contains? (cadr r) "`goldfish/liii` (library)"))
    (check-true (string-contains? (cadr r) "`xmake.lua` (config)"))
    (check-true (string-contains? (cadr r) "**build**: `xmake build` (xmake.lua)"))
  ) ;let
  (let ((r (%run-pi %work-scheme "--print" "--tour" "testing")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "## Code Tour: testing"))
    (check-true (string-contains? (cadr r) "tests/login-test.scm"))
    (check-true (string-contains? (cadr r) "Test file for login"))
  ) ;let
  (let ((r (%run-pi %work-scheme "--print" "--tour" "goldfish")))
    (check (car r) => 0)
    (check-true (string-contains? (cadr r) "goldfish/liii/base.scm"))
  ) ;let
) ;define

(define (%check-injection!)
  (display "=== 验收点 5: 系统提示词注入 ===\n")
  (let ((block (pi-compass-injection-text '() %work)))
    (check (string? block) => #t)
    (check-true (string-starts? block "\n\n## Codebase Map:"))
  ) ;let
  (check (pi-compass-injection-text '((no-codemap-inject . #t)) %work) => #f)
  (check (string-starts? (pi-compass-turn-system-prompt "BASE" '() %work)
           "BASE\n\n## Codebase Map:"
         ) ;string-starts?
    =>
    #t
  ) ;check
  ;; 无缓存工程不注入。
  (%setup-dir! %work-empty)
  (check (pi-compass-injection-text '() %work-empty) => #f)
  ;; 超过上限时截断并追加脚本模式提示。
  (let ((block
          (pi-compass-build-injection (%data (%read-entry (%cache-path %work))) 200)
        ) ;block
       ) ;
    (check-true (string-ends? block
                  "\n... (codemap truncated, run --codemap to see full output)"
                ) ;string-ends?
    ) ;check-true
  ) ;let
  ;; 缓存陈旧（哈希不匹配）不注入。
  (path-write-text (path (string-append %work-scheme "/src/new.scm"))
    "(define x 1)\n"
  ) ;path-write-text
  (check (pi-compass-injection-text '() %work-scheme) => #f)
) ;define

(define (%tool-text res)
  (cdr (assoc 'text (vector-ref (pi-tool-result-content res) 0)))
) ;define

(define (%check-tools!)
  (display "=== 验收点 6: 内置工具 ===\n")
  (check (pi-tool-definition-name (pi-codebase-map-definition)) => "codebase_map")
  (check (pi-tool-definition-name (pi-code-tour-definition)) => "code_tour")
  (let ((orig (getcwd)))
    (dynamic-wind (lambda () (chdir %work))
      (lambda ()
        (let ((res (pi-execute-codebase-map '())))
          (check (pi-tool-result? res) => #t)
          (check (pi-tool-result-is-error res) => #f)
          (check-true (string-starts? (%tool-text res) "## Codebase Map:"))
        ) ;let
        (let ((res (pi-execute-code-tour '())))
          (check (pi-tool-result-is-error res) => #f)
          (check-true (string-starts? (%tool-text res) "Available tour topics: "))
        ) ;let
        (let ((res (pi-execute-code-tour '(("topic" . "src")))))
          (check-true (string-starts? (%tool-text res) "## Code Tour: src"))
          (check-true (string-contains? (%tool-text res) "src/index.js"))
        ) ;let
        (let ((res (pi-execute-code-tour '(("topic" . "nope")))))
          (check (pi-tool-result-is-error res) => #f)
          (check (%tool-text res) => "No files found for topic \"nope\".")
        ) ;let
      ) ;lambda
      (lambda () (chdir orig))
    ) ;dynamic-wind
  ) ;let
  ;; 默认注册 / 可关 / 显式白名单不自动追加。
  (check (pi-compass-extend-tool-names '("read") '())
    =>
    '("read" "codebase_map" "code_tour")
  ) ;check
  (check (pi-compass-extend-tool-names '("read") '((no-tools . #t))) => '("read"))
  (check (pi-compass-extend-tool-names '("read") '((exclude-tools . "code_tour")))
    =>
    '("read" "codebase_map")
  ) ;check
  (check (pi-compass-extend-tool-names '("read") '((tools . "read")))
    =>
    '("read")
  ) ;check
) ;define

(define %compass-tests
  '("tools/pi/tests/liii/pi/compass/args/pi-parse-pi-args-compass-test.scm"
    "tools/pi/tests/liii/pi/compass/build/pi-compass-detect-build-scripts-test.scm"
    "tools/pi/tests/liii/pi/compass/conventions/pi-compass-detect-conventions-test.scm"
    "tools/pi/tests/liii/pi/compass/entry/pi-compass-detect-entry-points-test.scm"
    "tools/pi/tests/liii/pi/compass/frameworks/pi-compass-detect-frameworks-test.scm"
    "tools/pi/tests/liii/pi/compass/generate/pi-compass-generate-codemap-test.scm"
    "tools/pi/tests/liii/pi/compass/keyfiles/pi-compass-detect-key-files-test.scm"
    "tools/pi/tests/liii/pi/compass/packages/pi-compass-detect-packages-test.scm"
    "tools/pi/tests/liii/pi/compass/project/pi-compass-detect-project-test.scm"
    "tools/pi/tests/liii/pi/compass/prompt/pi-compass-build-injection-test.scm"
    "tools/pi/tests/liii/pi/compass/prompt/pi-compass-injection-text-test.scm"
    "tools/pi/tests/liii/pi/compass/render/pi-compass-render-codemap-test.scm"
    "tools/pi/tests/liii/pi/compass/render/pi-compass-truncate-codemap-test.scm"
    "tools/pi/tests/liii/pi/compass/store/pi-compass-get-or-generate-test.scm"
    "tools/pi/tests/liii/pi/compass/tools/pi-compass-extend-tool-names-test.scm"
    "tools/pi/tests/liii/pi/compass/tools/pi-execute-codebase-map-test.scm"
    "tools/pi/tests/liii/pi/compass/tour/pi-compass-build-tour-test.scm"
    "tools/pi/tests/liii/pi/compass/tour/pi-compass-describe-file-test.scm"
    "tools/pi/tests/liii/pi/compass/tour/pi-compass-detect-topics-test.scm"
    "tools/pi/tests/liii/pi/compass/tour/pi-compass-render-tour-test.scm"
    "tools/pi/tests/liii/pi/compass/tree/pi-compass-build-directory-tree-test.scm"
    "tools/pi/tests/liii/pi/compass/tree/pi-compass-format-directory-tree-test.scm"
    "tools/pi/tests/liii/pi/modes/compass/run-compass-flow-test.scm")
) ;define

(define (%check-tests!)
  (display "=== 验收点 9b: 23 个新增单元测试 ===\n")
  (check (length %compass-tests) => 23)
  (for-each
    (lambda (f)
      (check (%ok? "unit" (string-append "timeout 120 bin/gf test " f) 150) => 0)
    ) ;lambda
    %compass-tests
  ) ;for-each
  (display "=== 验收点 9c: CLI / JSON / 工具回归 ===\n")
  (for-each
    (lambda (f)
      (check (%ok? "regress" (string-append "timeout 120 bin/gf test " f) 150) => 0)
    ) ;lambda
    '("tools/pi/tests/liii/pi-args-test.scm"
      "tools/pi/tests/liii/pi-tools-basic-test.scm"
      "tools/pi/tests/liii/pi-prompt-test.scm"
      "tools/pi/tests/liii/pi-import-leak-test.scm"
      "tools/pi/tests/liii/pi-cli-e2e-test.scm"
      "tools/pi/tests/liii/pi-json-e2e-test.scm")
  ) ;for-each
) ;define

(define (%check-gates!)
  (display "=== 验收点 9: 构建 / fmt / check / doc 门禁 ===\n")
  (check (%ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
  (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
  (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
  (check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)
) ;define

(define (%check-line-limits!)
  (display "=== 验收点 9d: 单文件 <= 300 行 ===\n")
  (for-each
    (lambda (f)
      (check-true
        (<=
          (length (string-split (path-read-text (path f)) "\n"))
          300
        ) ;<=
      ) ;check-true
    ) ;lambda
    '("tools/pi/liii/pi/compass.scm" "tools/pi/liii/pi/compass/args.scm"
      "tools/pi/liii/pi/compass/build.scm"
      "tools/pi/liii/pi/compass/conventions.scm"
      "tools/pi/liii/pi/compass/entry.scm"
      "tools/pi/liii/pi/compass/frameworks.scm"
      "tools/pi/liii/pi/compass/generate.scm"
      "tools/pi/liii/pi/compass/keyfiles.scm"
      "tools/pi/liii/pi/compass/packages.scm"
      "tools/pi/liii/pi/compass/packages/read.scm"
      "tools/pi/liii/pi/compass/packages/toml.scm"
      "tools/pi/liii/pi/compass/project.scm"
      "tools/pi/liii/pi/compass/prompt.scm"
      "tools/pi/liii/pi/compass/render.scm" "tools/pi/liii/pi/compass/store.scm"
      "tools/pi/liii/pi/compass/tools.scm" "tools/pi/liii/pi/compass/tour.scm"
      "tools/pi/liii/pi/compass/tour/files.scm"
      "tools/pi/liii/pi/compass/tree.scm" "tools/pi/liii/pi/compass/types.scm"
      "tools/pi/liii/pi/modes/compass.scm" "tools/pi/liii/pi/args/files.scm"
      "tools/pi/devel/048.scm" "tools/pi/devel/048-support.scm"
      "tools/pi/devel/048-support-cli.scm")
  ) ;for-each
) ;define
