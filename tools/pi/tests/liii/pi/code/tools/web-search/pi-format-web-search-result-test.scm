(import (scheme base)
        (liii check)
        (liii string)
        (liii pi code tools web-search format))

(check-set-mode! 'report-failed)

;; 1. 空文本与空数据
(let ((res (pi-format-web-search-result "" '())))
  (check (cdr (assoc 'text res)) => "")
  (check (vector-length (cdr (assoc 'content res))) => 1)
  (check (cdr (assoc 'sources res)) => '())
  (let ((details (cdr (assoc 'details res))))
    (check (cdr (assoc 'grounded details)) => #f)
    (check (cdr (assoc 'resultCount details)) => 0)))

;; 2. 纯回答文本（无 grounding 数据）
(let ((res (pi-format-web-search-result "Scheme is a minimalist dialect." '())))
  (check (cdr (assoc 'text res)) => "Scheme is a minimalist dialect.")
  (check-false (string-contains? (cdr (assoc 'text res)) "## Sources"))
  (check-false (string-contains? (cdr (assoc 'text res)) "## Additional Search Results")))

;; 3. 带 citations 的回答文本：追加 ## Sources，按 [1] Title - URL 格式
(let* ((citations (list (list (cons 'title "Scheme R7RS")
                              (cons 'url "https://schemers.org/r7rs.html")
                              (cons 'citedText "Scheme is minimalist"))
                        (list (cons 'title "Goldfish")
                              (cons 'url "https://goldfish.liii.pro")
                              (cons 'citedText "Goldfish is fast"))))
       (grounding (list (cons 'citations citations)
                        (cons 'searchResults '())
                        (cons 'searchQueries '("scheme r7rs"))))
       (res (pi-format-web-search-result "Scheme is a great language." grounding)))
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "[1] Scheme R7RS - https://schemers.org/r7rs.html"))
    (check-true (string-contains? txt "[2] Goldfish - https://goldfish.liii.pro"))
    (check-false (string-contains? txt "## Additional Search Results")))
  (check (length (cdr (assoc 'sources res))) => 2)
  (let ((details (cdr (assoc 'details res))))
    (check (cdr (assoc 'grounded details)) => #t)
    (check (cdr (assoc 'resultCount details)) => 2)))

;; 4. 带未引用 searchResults 的回答文本：追加 ## Additional Search Results
(let* ((results (list (list (cons 'title "Scheme Wiki")
                            (cons 'url "https://community.schemewiki.org"))
                      (list (cons 'title "S7 Scheme")
                            (cons 'url "https://ccrma.stanford.edu/software/snd/snd/s7.html"))))
       (grounding (list (cons 'citations '())
                        (cons 'searchResults results)
                        (cons 'searchQueries '("scheme wiki"))))
       (res (pi-format-web-search-result "Overview of Scheme." grounding)))
  (let ((txt (cdr (assoc 'text res))))
    (check-false (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "## Additional Search Results"))
    (check-true (string-contains? txt "1. Scheme Wiki - https://community.schemewiki.org"))
    (check-true (string-contains? txt "2. S7 Scheme - https://ccrma.stanford.edu/software/snd/snd/s7.html"))))

;; 5. 混合：被引用的入 Sources，未引用的入 Additional Search Results（且去重）
(let* ((citations (list (list (cons 'title "Source A") (cons 'url "https://a.test"))))
       (results (list (list (cons 'title "Source A") (cons 'url "https://a.test"))
                      (list (cons 'title "Source B") (cons 'url "https://b.test"))
                      (list (cons 'title "Source B") (cons 'url "https://b.test"))))
       (grounding (list (cons 'citations citations)
                        (cons 'searchResults results)
                        (cons 'searchQueries '("test query"))))
       (res (pi-format-web-search-result "Answer text." grounding)))
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "[1] Source A - https://a.test"))
    (check-true (string-contains? txt "## Additional Search Results"))
    (check-true (string-contains? txt "1. Source B - https://b.test"))
    ;; 不应在 Additional 中出现已在 Sources 的 Source A
    (check-false (string-contains? txt "2. Source A"))))

;; 6. 原文已含 ## Sources，不重复追加
(let* ((orig "Pre-formatted answer.\n\n## Sources\n[1] Custom - https://custom.org")
       (citations (list (list (cons 'title "Ignore me") (cons 'url "https://ignore.org"))))
       (grounding (list (cons 'citations citations) (cons 'searchResults '())))
       (res (pi-format-web-search-result orig grounding)))
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "[1] Custom - https://custom.org"))
    (check-false (string-contains? txt "[1] Ignore me"))))

;; 7. 长度截断测试
(let* ((long-text (make-string 300 #\x))
       (res (pi-format-web-search-result long-text '() :max-bytes 100)))
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "[Truncated]"))
    (check-true (<= (string-length txt) 150))))

;; 8. 支持接收包含 groundingMetadata 的原始 JSON 字典
(let* ((raw-json (list (cons "groundingMetadata"
                             (list (cons "webSearchQueries" #("raw query"))
                                   (cons "groundingChunks"
                                         #(((web . ((title . "Raw Title")
                                                    (uri . "https://raw.test"))))))
                                   (cons "groundingSupports"
                                         #(((segment . ((text . "text") (endIndex . 4)))
                                            (groundingChunkIndices . #(0)))))))))
       (res (pi-format-web-search-result "Answer with raw json" raw-json)))
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "[1] Raw Title - https://raw.test"))))

(check-report)
