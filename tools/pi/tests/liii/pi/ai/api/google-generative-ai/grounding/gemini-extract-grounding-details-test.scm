(import (scheme base)
        (scheme file)
        (liii check)
        (liii json)
        (liii pi ai api google-generative-ai grounding))

(check-set-mode! 'report-failed)

(define (%fixture-path file)
  (cond ((file-exists? (string-append "tests/fixtures/" file))
         (string-append "tests/fixtures/" file))
        ((file-exists? (string-append "tools/pi/tests/fixtures/" file))
         (string-append "tools/pi/tests/fixtures/" file))
        (else (string-append "../../tools/pi/tests/fixtures/" file))))

(define (%read-fixture-json file)
  (let* ((p (open-input-file (%fixture-path file)))
         (obj (read-json p)))
    (close-port p)
    obj))

;; gemini-extract-grounding-details
;; 1. 从真实 fixture 文件解析非流式完整响应 JSON
(let* ((resp (%read-fixture-json "gemini/grounding-response.json"))
       (details (gemini-extract-grounding-details resp))
       (queries (gemini-grounding-search-queries details))
       (results (gemini-grounding-search-results details))
       (citations (gemini-grounding-citations details)))

  ;; a. searchQueries 检验
  (check (length queries) => 2)
  (check (car queries) => "goldfish scheme programming language")
  (check (cadr queries) => "goldfish scheme github")

  ;; b. searchResults 检验（解析 web.title, web.uri, 缺省 Unknown）
  (check (length results) => 3)
  (let ((r0 (car results))
        (r1 (cadr results))
        (r2 (caddr results)))
    (check (cdr (assoc 'title r0)) => "Goldfish Scheme Repository")
    (check (cdr (assoc 'url r0)) => "https://github.com/XmacsLabs/goldfish")
    (check (cdr (assoc 'source r0)) => "google.groundingChunks")
    (check (cdr (assoc 'type r0)) => "web")

    (check (cdr (assoc 'title r1)) => "Goldfish Scheme Official Documentation")
    (check (cdr (assoc 'url r1)) => "https://goldfish.scheme.org")

    ;; 缺省 title 为 Unknown
    (check (cdr (assoc 'title r2)) => "Unknown")
    (check (cdr (assoc 'url r2)) => "https://example.com/no-title"))

  ;; c. citations 检验（groundingSupports 关联 segment 与 chunk index）
  (check (length citations) => 3)
  (let ((c0 (car citations))
        (c1 (cadr citations))
        (c2 (caddr citations)))
    (check (cdr (assoc 'title c0)) => "Goldfish Scheme Repository")
    (check (cdr (assoc 'url c0)) => "https://github.com/XmacsLabs/goldfish")
    (check (cdr (assoc 'citedText c0)) => "Goldfish Scheme is a high-performance Scheme")
    (check (cdr (assoc 'startIndex c0)) => 0)
    (check (cdr (assoc 'endIndex c0)) => 50)
    (check (cdr (assoc 'source c0)) => "google.groundingSupports")
    (check (cdr (assoc 'type c0)) => "citation")

    (check (cdr (assoc 'title c1)) => "Goldfish Scheme Repository")
    (check (cdr (assoc 'url c1)) => "https://github.com/XmacsLabs/goldfish")
    (check (cdr (assoc 'citedText c1)) => "interpreter designed for extensibility and embedding.")
    (check (cdr (assoc 'startIndex c1)) => 51)
    (check (cdr (assoc 'endIndex c1)) => 98)

    (check (cdr (assoc 'title c2)) => "Goldfish Scheme Official Documentation")
    (check (cdr (assoc 'url c2)) => "https://goldfish.scheme.org")
    (check (cdr (assoc 'citedText c2)) => "interpreter designed for extensibility and embedding.")))

;; 2. 直接传入 candidate 对象或直接传入 groundingMetadata 对象
(let* ((resp (%read-fixture-json "gemini/grounding-response.json"))
       (cands (cdr (assoc "candidates" resp)))
       (cand (vector-ref cands 0))
       (gm (cdr (assoc "groundingMetadata" cand)))
       (d-cand (gemini-extract-grounding-details cand))
       (d-gm (gemini-extract-grounding-details gm)))
  (check (length (gemini-grounding-search-queries d-cand)) => 2)
  (check (length (gemini-grounding-search-results d-cand)) => 3)
  (check (length (gemini-grounding-citations d-cand)) => 3)

  (check (length (gemini-grounding-search-queries d-gm)) => 2)
  (check (length (gemini-grounding-search-results d-gm)) => 3)
  (check (length (gemini-grounding-citations d-gm)) => 3))

;; 3. 双键兼容：可用 symbol 键也可直接用 string 键访问
(let* ((d (gemini-extract-grounding-details '())))
  (check (cdr (assoc 'searchQueries d)) => '())
  (check (cdr (assoc "searchQueries" d)) => '())
  (check (cdr (assoc 'searchResults d)) => '())
  (check (cdr (assoc "searchResults" d)) => '())
  (check (cdr (assoc 'citations d)) => '())
  (check (cdr (assoc "citations" d)) => '()))

;; 4. 容错性测试：空、缺失、残缺结构不抛异常，安全返回空结构
(check (gemini-grounding-search-queries (gemini-extract-grounding-details #f)) => '())
(check (gemini-grounding-search-results (gemini-extract-grounding-details '(()))) => '())
(check (gemini-grounding-citations (gemini-extract-grounding-details '(("candidates" . #())))) => '())

;; 5. 残缺 chunk 与无效 support 索引过滤
(let* ((bad-gm (list (cons "groundingChunks"
                           (vector (list (cons "other" 123))
                                   (list (cons "web" (list (cons "uri" "https://test.com"))))))
                     (cons "groundingSupports"
                           (vector (list (cons "segment" (list (cons "text" "s")))
                                         (cons "groundingChunkIndices" #(999)))
                                   (list (cons "segment" (list (cons "text" "ok")))
                                         (cons "groundingChunkIndices" #(1)))))))
       (d (gemini-extract-grounding-details bad-gm))
       (results (gemini-grounding-search-results d))
       (citations (gemini-grounding-citations d)))
  ;; 只有包含 web 的有效 chunk 被解析
  (check (length results) => 1)
  (check (cdr (assoc 'url (car results))) => "https://test.com")
  ;; 越界的 chunk index 999 被安全忽略，只有 index 1 被解析
  (check (length citations) => 1)
  (check (cdr (assoc 'citedText (car citations))) => "ok")
  (check (cdr (assoc 'url (car citations))) => "https://test.com"))

;; 6. 去重功能：重复 chunk 与重复 citation 不重复收录
(let* ((dup-gm (list (cons "groundingChunks"
                           (vector (list (cons "web" (list (cons "uri" "https://a.com")
                                                           (cons "title" "A"))))
                                   (list (cons "web" (list (cons "uri" "https://a.com")
                                                           (cons "title" "A"))))))
                     (cons "groundingSupports"
                           (vector (list (cons "segment" (list (cons "text" "same")))
                                         (cons "groundingChunkIndices" #(0 1)))))))
       (d (gemini-extract-grounding-details dup-gm))
       (results (gemini-grounding-search-results d))
       (citations (gemini-grounding-citations d)))
  (check (length results) => 1)
  (check (length citations) => 1))

(check-report)
