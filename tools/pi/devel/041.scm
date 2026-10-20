(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii json)
  (liii path)
  (liii string)
  (liii subprocess)
  (liii pi)
  (liii pi gemini)
  (liii pi gemini grounding)
  (liii pi gemini grounding-parse)
) ;import

(check-set-mode! 'report-failed)

;; [041] 验收脚本：Gemini 搜索请求体构造与 Grounding 解析
;; 逐条对应 tools/pi/devel/041.md 的验收点：
;;   1) Gemini 搜索请求体 tools 构造与注入（gemini-inject-google-search / gemini-build-search-tools）
;;   2) groundingMetadata 核心三要素解析（searchQueries / searchResults / citations）
;;   3) 响应与流式分片兼容与健壮性（完整响应 / candidate / 直接 GM / 残缺容错）
;;   4) 三层导出均可用：(liii pi gemini grounding) / (liii pi gemini) / (liii pi)
;;   5) 范围约束：纯数据结构处理，不发起网络请求、无 pi-execute-web-search
;;   6) 门禁：构建 / fmt --check / check / 单元测试全绿

(define %tmp-files '())

(define (%fixture-path file)
  (cond ((file-exists? (string-append "tests/fixtures/" file))
         (string-append "tests/fixtures/" file)
        ) ;
        ((file-exists? (string-append "tools/pi/tests/fixtures/" file))
         (string-append "tools/pi/tests/fixtures/" file)
        ) ;
        (else (string-append "../../tools/pi/tests/fixtures/" file))
  ) ;cond
) ;define

(define (%capture tag cmd timeout)
  (let* ((tmp (string-append "tmp-041-" tag ".txt")))
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

(define (%read-fixture-json file)
  (let* ((p (open-input-file (%fixture-path file))) (obj (read-json p)))
    (close-port p)
    obj
  ) ;let*
) ;define

(dynamic-wind (lambda () #f)
  (lambda ()
    (display "=== 验收点 1: 请求体 tools 构造与注入 ===\n")
    ;; 1a. 空请求体与纯 contents 注入
    (let* ((req-empty (gemini-inject-google-search '()))
           (tools-empty (cdr (assoc "tools" req-empty)))
          ) ;
      (check-true (vector? tools-empty))
      (check (vector-length tools-empty) => 1)
      (check (cdr (assoc "googleSearch" (vector-ref tools-empty 0))) => '(()))
      (check (json->string req-empty) => "{\"tools\":[{\"googleSearch\":{}}]}")
    ) ;let*

    ;; 1b. 与已有 functionDeclarations 共存
    (let* ((fc (list (cons "name" "read") (cons "description" "Read file")))
           (tools-orig
             (vector (list (cons "functionDeclarations" (vector fc))))
           ) ;tools-orig
           (orig (list (cons "contents" #()) (cons "tools" tools-orig)))
           (injected (gemini-inject-google-search orig))
           (tools (cdr (assoc "tools" injected)))
          ) ;
      (check (vector-length tools) => 2)
      (check-true (pair? (assoc "functionDeclarations" (vector-ref tools 0))))
      (check (cdr (assoc "googleSearch" (vector-ref tools 1))) => '(()))
    ) ;let*

    ;; 1c. 幂等性：已包含 googleSearch 或 google_search 时不重复添加
    (let* ((tools-exist
             (vector (list (cons "googleSearch" '(()))))
           ) ;tools-exist
           (injected (gemini-inject-google-search (list (cons "tools" tools-exist))))
          ) ;
      (check (vector-length (cdr (assoc "tools" injected))) => 1)
    ) ;let*
    (let* ((tools-legacy
             (vector (list (cons "google_search" '(()))))
           ) ;tools-legacy
           (injected (gemini-inject-google-search (list (cons "tools" tools-legacy))))
          ) ;
      (check (vector-length (cdr (assoc "tools" injected))) => 1)
    ) ;let*

    ;; 1d. gemini-build-search-tools 构造函数
    (let ((t1 (gemini-build-search-tools)))
      (check (vector-length t1) => 1)
      (check (cdr (assoc "googleSearch" (vector-ref t1 0))) => '(()))
    ) ;let
    (let* ((decls (list (list (cons "name" "test"))))
           (t2 (gemini-build-search-tools decls))
          ) ;
      (check (vector-length t2) => 2)
      (check (cdr (assoc "googleSearch" (vector-ref t2 1))) => '(()))
    ) ;let*

    (display "=== 验收点 2: groundingMetadata 三要素解析 ===\n")
    (let* ((resp (%read-fixture-json "gemini/grounding-response.json"))
           (details (gemini-extract-grounding-details resp))
           (queries (gemini-grounding-search-queries details))
           (results (gemini-grounding-search-results details))
           (citations (gemini-grounding-citations details))
          ) ;
      ;; a. searchQueries 检验
      (check (length queries) => 2)
      (check (car queries) => "goldfish scheme programming language")
      (check (cadr queries) => "goldfish scheme github")

      ;; b. searchResults 检验
      (check (length results) => 3)
      (check (cdr (assoc 'title (car results))) => "Goldfish Scheme Repository")
      (check (cdr (assoc 'url (car results)))
        =>
        "https://github.com/XmacsLabs/goldfish"
      ) ;check
      (check (cdr (assoc 'source (car results))) => "google.groundingChunks")
      (check (cdr (assoc 'type (car results))) => "web")
      ;; 缺省 title 检验
      (check (cdr (assoc 'title (caddr results))) => "Unknown")
      (check (cdr (assoc 'url (caddr results))) => "https://example.com/no-title")

      ;; c. citations 检验
      (check (length citations) => 3)
      (check (cdr (assoc 'title (car citations))) => "Goldfish Scheme Repository")
      (check (cdr (assoc 'citedText (car citations)))
        =>
        "Goldfish Scheme is a high-performance Scheme"
      ) ;check
      (check (cdr (assoc 'startIndex (car citations))) => 0)
      (check (cdr (assoc 'endIndex (car citations))) => 50)
      (check (cdr (assoc 'source (car citations))) => "google.groundingSupports")
      (check (cdr (assoc 'type (car citations))) => "citation")
    ) ;let*

    (display "=== 验收点 3: 响应与流式分片兼容与容错 ===\n")
    ;; 3a. candidate 对象与直接 groundingMetadata 对象
    (let* ((resp (%read-fixture-json "gemini/grounding-response.json"))
           (cand (vector-ref (cdr (assoc "candidates" resp)) 0))
           (gm (cdr (assoc "groundingMetadata" cand)))
           (d-cand (gemini-extract-grounding-details cand))
           (d-gm (gemini-extract-grounding-details gm))
          ) ;
      (check (length (gemini-grounding-search-queries d-cand)) => 2)
      (check (length (gemini-grounding-search-results d-cand)) => 3)
      (check (length (gemini-grounding-citations d-cand)) => 3)
      (check (length (gemini-grounding-search-queries d-gm)) => 2)
      (check (length (gemini-grounding-search-results d-gm)) => 3)
      (check (length (gemini-grounding-citations d-gm)) => 3)
    ) ;let*

    ;; 3b. 容错性：空值、残缺、越界不抛异常
    (check (gemini-grounding-search-queries (gemini-extract-grounding-details #f))
      =>
      '()
    ) ;check
    (check (gemini-grounding-search-results (gemini-extract-grounding-details '(())))
      =>
      '()
    ) ;check
    (check
      (gemini-grounding-citations (gemini-extract-grounding-details '(("candidates"
                                                                       . #())))
      ) ;gemini-grounding-citations
      =>
      '()
    ) ;check

    (display "=== 验收点 4: 三层导出均可用 ===\n")
    (check-true (procedure? gemini-build-search-tools))
    (check-true (procedure? gemini-inject-google-search))
    (check-true (procedure? gemini-extract-grounding-details))
    (check-true (procedure? gemini-grounding-search-queries))
    (check-true (procedure? gemini-grounding-search-results))
    (check-true (procedure? gemini-grounding-citations))

    (display "=== 验收点 5: 范围约束门禁 ===\n")
    ;; 本阶段不应包含网络执行器（留待 042）
    (check (cdr (%capture "grep-exec"
                  "grep -rn \"pi-execute-web-search\" tools/pi/liii || true"
                  60
                ) ;%capture
           ) ;cdr
      =>
      ""
    ) ;check

    (display "=== 验收点 6: 门禁与新增测试全绿 ===\n")
    (check (%ok? "build" "timeout 280 xmake b goldfish" 300) => 0)
    (check (%ok? "fmt" "timeout 120 bin/gf fmt --check" 150) => 0)
    (check (%ok? "check" "timeout 120 bin/gf check" 150) => 0)
    (check (%ok? "doc" "timeout 120 bin/gf doc --build-json" 150) => 0)

    ;; 300 行硬约束检查
    (check (%ok? "lines"
             "for f in tools/pi/liii/pi/gemini/grounding*.scm; do [ $(wc -l < \"$f\") -le 300 ] || exit 1; done"
             30
           ) ;%ok?
      =>
      0
    ) ;check

    (for-each
      (lambda (file)
        (check (%ok? "test" (string-append "timeout 120 bin/gf test " file) 150) => 0)
      ) ;lambda
      '("tools/pi/tests/liii/pi/gemini/grounding/gemini-inject-google-search-test.scm"
        "tools/pi/tests/liii/pi/gemini/grounding/gemini-build-search-tools-test.scm"
        "tools/pi/tests/liii/pi/gemini/grounding/gemini-extract-grounding-details-test.scm"
        "tools/pi/tests/liii/pi/pi-web-search-schema-test.scm"
        "tools/pi/tests/liii/pi/pi-validate-web-search-args-test.scm")
    ) ;for-each
  ) ;lambda
  (lambda () (%cleanup!))
) ;dynamic-wind

(check-report)
