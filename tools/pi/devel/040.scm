(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii path)
  (liii string)
  (liii subprocess)
  (liii pi)
  (liii pi tools)
  (liii pi web-search)
  (liii pi gemini)
  (liii pi args)
  (liii pi config)
) ;import

(check-set-mode! 'report-failed)

;; [040] 验收脚本：web_search 工具声明与 Schema 注册
;; 逐条对应 tools/pi/devel/040.md 的验收点：
;;   1) Schema 形状对齐原生 pi（object / query 必填 string / urls 可选 array maxItems 20）
;;   2) 三层导出：(liii pi web-search)、(liii pi tools)、(liii pi) 均可用
;;   3) 可选工具列表注册：PI_ALL_TOOL_NAMES 含 web_search，默认工具集不含
;;   4) 参数校验：合法通过，非法给 type-error / value-error
;;   5) 无回归 + 门禁：build / fmt --check / check + 相关测试
;;   6) 范围约束：不发起网络、不改请求体、尚无执行器
;; 外部命令一律经 (liii subprocess) run 执行并必带 :timeout。
;;
;; 说明：验收阶段代码尚未提交，故用 `bin/gf fmt --check`（等价于 fmt + 无 diff）
;; 与 `bin/gf check` 代替对工作区 git diff 的断言。

(define %tmp-files '())

(define (%capture tag cmd timeout)
  (let* ((tmp (string-append "tmp-040-" tag ".txt")))
    (set! %tmp-files (cons tmp %tmp-files))
    (let* ((ret (run (list (pi-find-bash-path) "-c" (string-append "(" cmd ") > " tmp " 2>&1"))
                  :stdin   'null
                  :timeout timeout
                ) ;run
           ) ;ret
           (out
             (if (and (file-exists? tmp) (path-file? (path tmp))) (path-read-text tmp) "")
           ) ;out
          ) ;
      (cons ret out)
    ) ;let*
  ) ;let*
) ;define

(define (%ok? tag cmd timeout)
  (let* ((res (%capture tag cmd timeout)) (rc (car res)))
    (unless (zero? rc)
      (display (string-append "!!! " tag " exited " (number->string rc) "\n" (cdr res))
      ) ;display
    ) ;unless
    rc
  ) ;let*
) ;define

(define (%cleanup!)
  (for-each (lambda (f) (when (file-exists? f) (delete-file f))) %tmp-files)
  (set! %tmp-files '())
) ;define

;; 20 个合法 URL（恰好达到 maxItems 上限）

(define (%urls n)
  (map (lambda (i) (string-append "https://example.com/" (number->string i)))
    (iota n)
  ) ;map
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (display "=== 验收点 1: Schema 形状对齐原生 pi ===\n")
    (let* ((schema (pi-web-search-schema))
           (props (cdr (assoc "properties" schema)))
           (query (cdr (assoc "query" props)))
           (urls (cdr (assoc "urls" props)))
          ) ;
      (check (cdr (assoc "type" schema)) => "object")
      (check (cdr (assoc "required" schema)) => #("query"))
      (check (cdr (assoc "type" query)) => "string")
      (check (cdr (assoc "description" query))
        =>
        "The search query or question to answer"
      ) ;check
      (check (cdr (assoc "type" urls)) => "array")
      (check (cdr (assoc "maxItems" urls)) => 20)
      (check
        (cdr (assoc "type" (cdr (assoc "items" urls))))
        =>
        "string"
      ) ;check
      (check (cdr (assoc "description" urls))
        =>
        "Additional URLs to analyze along with search (up to 20)"
      ) ;check
      ;; urls 是可选参数，不在 required 中
      (check-true
        (not
          (member "urls" (vector->list (cdr (assoc "required" schema))))
        ) ;not
      ) ;check-true
    ) ;let*

    (display "=== 验收点 2: 三层导出均可用 ===\n")
    ;; 本脚本同时 import 三层；若任一导出缺失/冲突会在编译期失败
    (check-true (procedure? pi-web-search-schema))
    (check-true (procedure? pi-validate-web-search-args))
    (check-true (pair? (pi-web-search-schema)))

    (display "=== 验收点 3: 可选工具列表注册 ===\n")
    (check (length PI_ALL_TOOL_NAMES) => 8)
    (check (member "web_search" PI_ALL_TOOL_NAMES) => '("web_search"))
    (check-true (not (member "web_search" PI_DEFAULT_TOOL_NAMES)))
    (check (parse-tools-selection #f) => '("read" "bash" "edit" "write"))
    (check (parse-tools-selection "+web_search")
      =>
      '("read" "bash" "edit" "write" "web_search")
    ) ;check
    (check (parse-tools-selection "read,web_search") => '("read" "web_search"))
    (check (parse-tools-selection "web_search") => '("web_search"))

    (display "=== 验收点 4: 参数校验 ===\n")
    (check (pi-validate-web-search-args '(("query" . "goldfish scheme"))) => #t)
    (check (pi-validate-web-search-args '(("query" . "q") ("urls"))) => #t)
    (check
      (pi-validate-web-search-args (list (cons "query" "q") (cons "urls" (%urls 20))))
      =>
      #t
    ) ;check
    ;; query 缺失 / 非 string
    (check-catch 'type-error (pi-validate-web-search-args '()))
    (check-catch 'type-error
      (pi-validate-web-search-args '(("urls" "https://example.com")))
    ) ;check-catch
    (check-catch 'type-error (pi-validate-web-search-args '(("query" . 42))))
    ;; query 空串 / 纯空白
    (check-catch 'value-error (pi-validate-web-search-args '(("query" . ""))))
    (check-catch 'value-error (pi-validate-web-search-args '(("query" . "   "))))
    ;; urls 非 list / 元素非 string
    (check-catch 'type-error
      (pi-validate-web-search-args '(("query" . "q") ("urls" . "not-a-list")))
    ) ;check-catch
    (check-catch 'type-error
      (pi-validate-web-search-args '(("query" . "q") ("urls" 42)))
    ) ;check-catch
    (check-catch 'type-error
      (pi-validate-web-search-args '(("query" . "q") ("urls" "ok" 42)))
    ) ;check-catch
    ;; urls 超过 20 项
    (check-catch 'value-error
      (pi-validate-web-search-args (list (cons "query" "q") (cons "urls" (%urls 21))))
    ) ;check-catch

    (display "=== 验收点 4b: --tools web_search 参数解析与选择 ===\n")
    ;; settings 必须是 alist（resolve-active-tools 内部对 assoc 结果取 cdr）；
    ;; 用 list/cons 构造，避免源码字面量被 gf fmt 改写成扁平列表而改变语义
    (let* ((settings (list (cons 'default-tools '("read" "bash" "edit" "write"))))
           (parsed (parse-pi-args '("--tools" "web_search")))
           (active (resolve-active-tools parsed settings))
          ) ;
      (check (pi-args-ref parsed 'tools) => "web_search")
      (check active => '("web_search"))
      ;; 增量选择：默认四件套 + web_search
      (check (resolve-active-tools (parse-pi-args '("--tools" "+web_search")) settings)
        =>
        '("read" "bash" "edit" "write" "web_search")
      ) ;check
      ;; 与 --exclude-tools 组合：排除 edit 后仍保留 web_search
      (check (resolve-active-tools (parse-pi-args '("--tools" "+web_search"
                                                    "--exclude-tools" "edit"))
               settings
             ) ;resolve-active-tools
        =>
        '("read" "bash" "write" "web_search")
      ) ;check
      ;; --no-tools 优先，选择结果为空
      (check (resolve-active-tools (parse-pi-args '("--tools"
                                                    "web_search"
                                                    "--no-tools"))
               settings
             ) ;resolve-active-tools
        =>
        '()
      ) ;check
      ;; settings 中的 default-tools 增量写法同样能带入 web_search
      (check
        (resolve-active-tools (parse-pi-args '())
          (list (cons 'default-tools '("+web_search")))
        ) ;resolve-active-tools
        =>
        '("read" "bash" "edit" "write" "web_search")
      ) ;check
    ) ;let*

    (display "=== 验收点 6: 范围约束（无网络/无请求体改动/无执行器） ===\n"
    ) ;display
    ;; 执行器属 042/043，本阶段不注册
    (check (get-tool-definition "web_search") => #f)
    ;; 未注册执行器时请求构造必须跳过该工具，避免发出不可执行声明
    (check (gemini-build-tool-declarations '("web_search")) => '())
    (check (length (gemini-build-tool-declarations '("read"))) => 1)
    ;; 静态门禁：无执行器、无网络调用、无 Grounding/googleSearch 解析
    (check (cdr (%capture "grep-exec"
                  "grep -rn \"pi-execute-web-search\" tools/pi/liii || true"
                  60
                ) ;%capture
           ) ;cdr
      =>
      ""
    ) ;check
    (check (cdr (%capture "grep-http"
                  "grep -n \"(liii http)\" tools/pi/liii/pi/web-search.scm || true"
                  60
                ) ;%capture
           ) ;cdr
      =>
      ""
    ) ;check
    (check (cdr (%capture "grep-grounding"
                  "grep -rn \"googleSearch\\|groundingMetadata\" tools/pi/liii || true"
                  60
                ) ;%capture
           ) ;cdr
      =>
      ""
    ) ;check

    (display "=== 验收点 5a: 门禁（构建 / fmt / check） ===\n")
    (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
    (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
    (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)

    (display "=== 验收点 5b: 新增测试与相关回归 ===\n")
    (for-each
      (lambda (file)
        (check (%ok? "test" (string-append "timeout 120 bin/gf test " file) 150) => 0)
      ) ;lambda
      '("tools/pi/tests/liii/pi/pi-web-search-schema-test.scm"
        "tools/pi/tests/liii/pi/pi-validate-web-search-args-test.scm"
        "tools/pi/tests/liii/pi-tools-basic-test.scm"
        "tools/pi/tests/liii/pi-tools-search-test.scm"
        "tools/pi/tests/liii/pi-import-leak-test.scm")
    ) ;for-each
  ) ;lambda
  (lambda () (%cleanup!))
) ;dynamic-wind

(check-report)
