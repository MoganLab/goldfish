(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii json)
  (liii subprocess)
  (liii pi bash)
  (liii tui)
  (liii tui component)
) ;import

(check-set-mode! 'report-failed)

;; [054] 验收脚本：(liii tui component) 核心协议与基础文本组件
;; 逐条对应 tools/pi/devel/054.md 验收点：
;;   1) 门禁检查：构建 / fmt / git diff --exit-code / check / doc
;;   2) 单文件行数 <= 300 行硬约束（代码与测试文件）
;;   3) 9 个单函数单元测试 CLI 运行全量通过
;;   4) Component 核心协议进程内行为（谓词、调度、错误处理）
;;   5) make-text 自动折行换行文本组件语义断言
;;   6) make-truncated-text 单行截断组件语义断言
;;   7) make-spacer 占位组件语义断言
;;   8) 文档索引注册验证

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-054-" tag ".txt")))
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

(display "=== 验收点 1: 门禁检查（构建 / fmt / diff / check / doc） ===\n"
) ;display
(check (%run-ok? "build" "timeout 300 xmake b goldfish" 300) => 0)
(check (%run-ok? "fmt" "timeout 120 bin/gf fmt" 120) => 0)
(check (%run-ok? "diff" "git diff --exit-code goldfish/ tests/" 120) => 0)
(check (%run-ok? "check" "timeout 120 bin/gf check" 120) => 0)
(check (%run-ok? "doc" "timeout 120 bin/gf doc --build-json" 120) => 0)

(display "=== 验收点 2: 任务 054 相关代码与测试文件行数硬约束 (<= 300 行) ===\n"
) ;display
(let ((files '("goldfish/liii/tui.scm" "goldfish/liii/tui/component.scm"
               "tests/liii/tui-test.scm"
               "tests/liii/tui/component/component-p-test.scm"
               "tests/liii/tui/component/make-component-test.scm"
               "tests/liii/tui/component/component-render-test.scm"
               "tests/liii/tui/component/component-handle-input-test.scm"
               "tests/liii/tui/component/component-invalidate-test.scm"
               "tests/liii/tui/component/make-text-test.scm"
               "tests/liii/tui/component/make-truncated-text-test.scm"
               "tests/liii/tui/component/make-spacer-test.scm"
               "tools/pi/devel/054.scm")
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

(display "=== 验收点 3: 9 个单函数单元测试 CLI 执行验证 ===\n")
(check (%run-ok? "t-tui" "timeout 120 bin/gf tests/liii/tui-test.scm" 120) => 0)
(check (%run-ok? "t-comp-p"
         "timeout 120 bin/gf tests/liii/tui/component/component-p-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-make-comp"
         "timeout 120 bin/gf tests/liii/tui/component/make-component-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-render"
         "timeout 120 bin/gf tests/liii/tui/component/component-render-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-handle-in"
         "timeout 120 bin/gf tests/liii/tui/component/component-handle-input-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-inval"
         "timeout 120 bin/gf tests/liii/tui/component/component-invalidate-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-text"
         "timeout 120 bin/gf tests/liii/tui/component/make-text-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-trunc"
         "timeout 120 bin/gf tests/liii/tui/component/make-truncated-text-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check
(check (%run-ok? "t-spacer"
         "timeout 120 bin/gf tests/liii/tui/component/make-spacer-test.scm"
         120
       ) ;%run-ok?
  =>
  0
) ;check

(display "=== 验收点 4: Component 核心协议进程内行为断言 ===\n")
(let ((comp
        (make-component
          (lambda (w) (list (string-append "width=" (number->string w))))
        ) ;make-component
      ) ;comp
     ) ;
  (check-true (component? comp))
  (check (component-render comp 80) => '("width=80"))
  (check (component-handle-input comp 'enter) => #f)
  (check (component-invalidate! comp) => #t)
) ;let

;; 验证位置传参和关键字传参构造
(let* ((invalidated #f)
       (comp
         (make-component
           :render       (lambda (c w) (list "custom-render"))
           :handle-input (lambda (c key) (if (eq? key 'q) 'quit #f))
           :invalidate   (lambda (c) (set! invalidated #t) 'ok)
         ) ;make-component
       ) ;comp
      ) ;
  (check-true (component? comp))
  (check (component-render comp 40) => '("custom-render"))
  (check (component-handle-input comp 'q) => 'quit)
  (check (component-handle-input comp 'tab) => #f)
  (check (component-invalidate! comp) => 'ok)
  (check-true invalidated)
) ;let*

;; 验证参数错误拦截
(check-catch 'type-error (make-component "not-a-procedure"))
(check-catch 'type-error (component-render "not-a-comp" 80))
(check-catch 'type-error (component-render (make-spacer 1) "not-a-width"))
(check-catch 'type-error (component-handle-input 123 'enter))
(check-catch 'type-error (component-invalidate! 'not-a-comp))

(display "=== 验收点 5: make-text 自动折行文本组件语义断言 ===\n"
) ;display
(let ((t (make-text "Goldfish Scheme provides lightweight and fast Scheme runtime")))
  (check-true (component? t))
  (check (component-render t 25)
    =>
    '("Goldfish Scheme provides" "lightweight and fast" "Scheme runtime")
  ) ;check
  (check (component-render t 0) => '(""))
  (check (component-render t -10) => '(""))
) ;let

;; 验证包含换行符的分段折行
(let ((t2 (make-text "Line one\nLine two with several words\nLine three")))
  (check (component-render t2 15)
    =>
    '("Line one" "Line two with" "several words" "Line three")
  ) ;check
) ;let

;; 验证空文本和类型保护
(let ((empty-t (make-text "")))
  (check (component-render empty-t 80) => '(""))
) ;let
(check-catch 'type-error (make-text 123))

(display "=== 验收点 6: make-truncated-text 单行截断组件语义断言 ===\n"
) ;display
(let ((tr (make-truncated-text "Hello, World! Welcome to Goldfish.")))
  (check-true (component? tr))
  (check (component-render tr 12) => '("Hello, Wo..."))
  (check (component-render tr 50) => '("Hello, World! Welcome to Goldfish."))
  (check (component-render tr 3) => '("..."))
  (check (component-render tr 2) => '(".."))
  (check (component-render tr 1) => '("."))
  (check (component-render tr 0) => '(""))
) ;let

;; 验证自定义省略符与多行文本截取首行
(let ((tr2 (make-truncated-text "first line is very long\nsecond line" " [more]")))
  (check (component-render tr2 15) => '("first li [more]"))
) ;let
(check-catch 'type-error (make-truncated-text 456))
(check-catch 'type-error (make-truncated-text "abc" 789))

(display "=== 验收点 7: make-spacer 占位组件语义断言 ===\n")
(let ((sp1 (make-spacer)) (sp3 (make-spacer 3)) (sp0 (make-spacer 0)))
  (check-true (component? sp1))
  (check (component-render sp1 80) => '(""))
  (check (component-render sp3 80) => '("" "" ""))
  (check (component-render sp0 80) => '())
) ;let
(check-catch 'type-error (make-spacer "1"))
(check-catch 'value-error (make-spacer -1))

(display "=== 验收点 8: 文档索引与元数据注册验证 ===\n")
(let* ((index-path "tests/function-library-index.json")
       (index-content (path-read-text index-path))
      ) ;
  (check-true (string-contains? index-content "make-text"))
  (check-true (string-contains? index-content "make-truncated-text"))
  (check-true (string-contains? index-content "make-spacer"))
  (check-true (string-contains? index-content "component-render"))
  (check-true (string-contains? index-content "(liii tui component)"))
  (check-true (string-contains? index-content "(liii tui)"))
) ;let*

(check-report)
