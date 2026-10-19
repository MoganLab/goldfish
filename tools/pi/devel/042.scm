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
  (liii pi web-search format)
  (liii pi web-search execute)
  (liii pi web-search schema)
) ;import

(check-set-mode! 'report-failed)

;; [042] 验收脚本：web_search 网络执行器与结果格式化
;; 逐条对应 tools/pi/devel/042.md 的验收点：
;;   1) 回答内容与引用链接拼装格式化（pi-format-web-search-result）
;;   2) 网络执行器参数校验与鉴权错误处理（pi-execute-web-search）
;;   3) Stub Server 错误路径全覆盖（401 / 403 / 500 / 非 JSON / 空 candidates / 超时）
;;   4) 工具分发注册：get-tool-definition / execute-tool-call 支持 web_search
;;   5) 门禁与新增测试全绿：构建 / fmt --check / check / doc / 单测 / 300 行约束

(define %tmp-files '())

(define (%capture tag cmd timeout)
  (let* ((tmp (string-append "tmp-042-" tag ".txt")))
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

(define (find-stub-server)
  (let loop
    ((candidates '("tools/pi/tests/stub_gemini_server.py"
                   "tests/stub_gemini_server.py" "stub_gemini_server.py"
                   "../stub_gemini_server.py" "../../stub_gemini_server.py")
     ) ;candidates
    ) ;
    (cond ((null? candidates) (error 'file-not-found "stub_gemini_server.py not found"))
          ((file-exists? (car candidates)) (car candidates))
          (else (loop (cdr candidates)))
    ) ;cond
  ) ;let
) ;define

(define (parse-port-line line)
  (if (and (string? line) (string-starts? line "PORT:"))
    (string->number (substring line 5 (string-length line)))
    #f
  ) ;if
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (display "=== 验收点 1: 回答内容与引用链接拼装格式化 ===\n")
    (let ((res (pi-format-web-search-result "" '())))
      (check (cdr (assoc 'text res)) => "")
      (check (vector-length (cdr (assoc 'content res))) => 1)
      (check (cdr (assoc 'sources res)) => '())
      (check
        (cdr (assoc 'grounded (cdr (assoc 'details res))))
        =>
        #f
      ) ;check
    ) ;let
    (let ((res (pi-format-web-search-result "Plain answer without grounding." '())))
      (check (cdr (assoc 'text res)) => "Plain answer without grounding.")
      (check-false (string-contains? (cdr (assoc 'text res)) "## Sources"))
    ) ;let

    ;; 带 citations 与 searchResults 拼装
    (let* ((cits (list (list (cons 'title "Doc A") (cons 'url "https://a.org") (cons 'citedText "t-a"))
                   (list (cons 'title "Doc B") (cons 'url "https://b.org") (cons 'citedText "t-b"))
                 ) ;list
           ) ;cits
           (g-cit (list (cons 'citations cits)
                    (cons 'searchResults '())
                    (cons 'searchQueries '("q"))
                  ) ;list
           ) ;g-cit
           (res-cit (pi-format-web-search-result "Answer." g-cit))
           (txt-cit (cdr (assoc 'text res-cit)))
          ) ;
      (check-true (string-contains? txt-cit "## Sources"))
      (check-true (string-contains? txt-cit "[1] Doc A - https://a.org"))
      (check-true (string-contains? txt-cit "[2] Doc B - https://b.org"))
      (check-false (string-contains? txt-cit "## Additional Search Results"))
      (check (length (cdr (assoc 'sources res-cit))) => 2)
      (check
        (cdr (assoc 'grounded (cdr (assoc 'details res-cit))))
        =>
        #t
      ) ;check
    ) ;let*

    (let* ((extra (list (list (cons 'title "Extra 1") (cons 'url "https://x1.org"))
                    (list (cons 'title "Extra 2") (cons 'url "https://x2.org"))
                  ) ;list
           ) ;extra
           (g-ext (list (cons 'citations '())
                    (cons 'searchResults extra)
                    (cons 'searchQueries '("q"))
                  ) ;list
           ) ;g-ext
           (res-ext (pi-format-web-search-result "Extra." g-ext))
           (txt-ext (cdr (assoc 'text res-ext)))
          ) ;
      (check-false (string-contains? txt-ext "## Sources"))
      (check-true (string-contains? txt-ext "## Additional Search Results"))
      (check-true (string-contains? txt-ext "1. Extra 1 - https://x1.org"))
    ) ;let*

    ;; 截断防护
    (let ((res (pi-format-web-search-result (make-string 500 #\y) '() :max-bytes 100)))
      (check-true (string-contains? (cdr (assoc 'text res)) "[Truncated]"))
    ) ;let

    (display "=== 验收点 2: 网络执行器参数校验与鉴权错误处理 ===\n"
    ) ;display
    (check-catch 'type-error (pi-execute-web-search '()))
    (check-catch 'type-error (pi-execute-web-search '(("query" . 123))))
    (check-catch 'value-error (pi-execute-web-search '(("query" . "   "))))
    (check-catch 'type-error
      (pi-execute-web-search '(("query" . "q") ("urls" . "bad")))
    ) ;check-catch
    (check-catch 'type-error
      (pi-execute-web-search '(("query" . "q") ("urls" (123))))
    ) ;check-catch
    (check-catch 'error (pi-execute-web-search '(("query" . "q")) :api-key ""))

    (display "=== 验收点 3: Stub Server 真实交互与各错误路径全覆盖 ===\n"
    ) ;display
    (let* ((stub-py (find-stub-server))
           (proc (process-spawn (list "python3" stub-py) :stdout 'pipe))
           (port (parse-port-line (process-read-line proc)))
           (base-url (string-append "http://127.0.0.1:" (number->string port)))
           (run-s (lambda (args mid key tout)
                    (pi-execute-web-search args
                      :base-url base-url
                      :model-id mid
                      :api-key  key
                      :timeout  tout
                    ) ;pi-execute-web-search
                  ) ;lambda
           ) ;run-s
          ) ;
      (check-true (number? port))

      ;; 3a. 正常搜索与 3b. 带 URLs
      (let* ((r1 (run-s '(("query" . "goldfish scheme")) "web-search-basic" "k" 10))
             (t1 (cdr (assoc 'text r1)))
            ) ;
        (check-false (cdr (assoc 'isError r1)))
        (check-true (string-contains? t1 "Goldfish Scheme is a minimalist"))
        (check-true (string-contains? t1 "## Sources"))
        (check (length (cdr (assoc 'sources r1))) => 1)
      ) ;let*
      (let* ((r2 (run-s '(("query" . "cmp")
                          ("urls" "https://goldfish.liii.pro"))
                   "web-search-urls"
                   "k"
                   10
                 ) ;run-s
             ) ;r2
            ) ;
        (check-false (cdr (assoc 'isError r2)))
        (check-true (string-contains? (cdr (assoc 'text r2)) "prompt_has_urls=True"))
      ) ;let*

      ;; 3c-3f. 401 / 403 / 500 / 非 JSON 错误
      (check-catch 'error (run-s '(("query" . "q")) "web-search-401" "bad" 10))
      (check-catch 'error (run-s '(("query" . "q")) "web-search-403" "k" 10))
      (check-catch 'error (run-s '(("query" . "q")) "error-500" "k" 10))
      (check-catch 'error (run-s '(("query" . "q")) "web-search-non-json" "k" 10))

      ;; 3g. 空 candidates 降级
      (let ((r-empty (run-s '(("query" . "emp")) "web-search-empty-candidates" "k" 10)))
        (check-false (cdr (assoc 'isError r-empty)))
        (check-true (string-contains? (cdr (assoc 'text r-empty)) "No search results"))
      ) ;let

      ;; 3h. 超时控制
      (check-catch 'error (run-s '(("query" . "slow")) "web-search-hang" "k" 1))

      ;; 3i. execute-tool-call 分发
      (let ((call-res (execute-tool-call "web_search"
                        '(("query" . "goldfish scheme"))
                        :base-url base-url
                        :model-id "web-search-basic"
                        :api-key  "k"
                        :timeout  10
                      ) ;execute-tool-call
            ) ;call-res
           ) ;
        (check-false (cdr (assoc 'isError call-res)))
        (check-true (string-contains? (cdr (assoc 'text call-res)) "Goldfish Scheme"))
      ) ;let

      (process-kill proc)
      (process-wait proc)
    ) ;let*

    (display "=== 验收点 4: 工具分发注册 ===\n")
    (let ((tool-def (get-tool-definition "web_search")))
      (check-true (pi-tool-definition? tool-def))
      (check (pi-tool-definition-name tool-def) => "web_search")
      (check (pi-tool-definition-description tool-def)
        =>
        "Search the web using Gemini Google Search grounding."
      ) ;check
      (check-true (pair? (member "web_search" PI_ALL_TOOL_NAMES)))
    ) ;let

    (display "=== 验收点 5: 门禁与新增测试全绿 ===\n")
    (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
    (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
    (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
    (check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)
    (check (%ok? "lines"
             "for f in tools/pi/liii/pi/web-search*.scm tools/pi/liii/pi/web-search/*.scm; do [ $(wc -l < \"$f\") -le 300 ] || exit 1; done"
             30
           ) ;%ok?
      =>
      0
    ) ;check

    (for-each
      (lambda (file)
        (check (%ok? "test" (string-append "timeout 120 bin/gf test " file) 150) => 0)
      ) ;lambda
      '("tools/pi/tests/liii/pi/web-search/pi-format-web-search-result-test.scm"
        "tools/pi/tests/liii/pi/web-search/pi-web-search-execute-test.scm"
        "tools/pi/tests/liii/pi/pi-web-search-schema-test.scm"
        "tools/pi/tests/liii/pi/pi-validate-web-search-args-test.scm")
    ) ;for-each
  ) ;lambda
  (lambda () (%cleanup!) (check-report))
) ;dynamic-wind
