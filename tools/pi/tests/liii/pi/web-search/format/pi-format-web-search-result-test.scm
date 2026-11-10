;; pi-format-web-search-result
;; 将联网搜索结果整理为回答文本、内容块与来源列表。
;;
;; 语法
;; ----
;; (pi-format-web-search-result text grounding)
;;
;; 参数
;; ----
;; text : string?
;; 模型返回的回答文本。
;;
;; grounding : list?
;; grounding 元数据，包含 citations 与搜索结果条目。
;;
;; 返回值
;; ----
;; list?
;; 包含 text、content、sources 与 details 字段的结果 alist。

(import (scheme base) (liii check) (liii string) (liii pi web-search format))

(check-set-mode! 'report-failed)

;; 1. 空文本与空数据
(let ((res (pi-format-web-search-result "" '())))
  (check (cdr (assoc 'text res)) => "")
  (check (vector-length (cdr (assoc 'content res))) => 1)
  (check (cdr (assoc 'sources res)) => '())
  (let ((details (cdr (assoc 'details res))))
    (check (cdr (assoc 'grounded details)) => #f)
    (check (cdr (assoc 'resultCount details)) => 0)
  ) ;let
) ;let

;; 2. 纯回答文本（无 grounding 数据）
(let ((res (pi-format-web-search-result "Scheme is a minimalist dialect." '())))
  (check (cdr (assoc 'text res)) => "Scheme is a minimalist dialect.")
  (check-false (string-contains? (cdr (assoc 'text res)) "## Sources"))
  (check-false (string-contains? (cdr (assoc 'text res)) "## Additional Search Results")
  ) ;check-false
) ;let

;; 3. 带 citations 的回答文本：追加 ## Sources，按 [1] Title - URL 格式
(let* ((citations (list (list (cons 'title "Scheme R7RS")
                          (cons 'url "https://schemers.org/r7rs.html")
                          (cons 'citedText "Scheme is minimalist")
                        ) ;list
                    (list (cons 'title "Goldfish")
                      (cons 'url "https://goldfish.liii.pro")
                      (cons 'citedText "Goldfish is fast")
                    ) ;list
                  ) ;list
       ) ;citations
       (grounding (list (cons 'citations citations)
                    (cons 'searchResults '())
                    (cons 'searchQueries '("scheme r7rs"))
                  ) ;list
       ) ;grounding
       (res (pi-format-web-search-result "Scheme is a great language." grounding))
      ) ;
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "[1] Scheme R7RS - https://schemers.org/r7rs.html")
    ) ;check-true
    (check-true (string-contains? txt "[2] Goldfish - https://goldfish.liii.pro"))
    (check-false (string-contains? txt "## Additional Search Results"))
  ) ;let
  (check (length (cdr (assoc 'sources res))) => 2)
  (let ((details (cdr (assoc 'details res))))
    (check (cdr (assoc 'grounded details)) => #t)
    (check (cdr (assoc 'resultCount details)) => 2)
  ) ;let
) ;let*

;; 4. 带未引用 searchResults 的回答文本：追加 ## Additional Search Results
(let* ((results (list (list (cons 'title "Scheme Wiki")
                        (cons 'url "https://community.schemewiki.org")
                      ) ;list
                  (list (cons 'title "S7 Scheme")
                    (cons 'url "https://ccrma.stanford.edu/software/snd/snd/s7.html")
                  ) ;list
                ) ;list
       ) ;results
       (grounding (list (cons 'citations '())
                    (cons 'searchResults results)
                    (cons 'searchQueries '("scheme wiki"))
                  ) ;list
       ) ;grounding
       (res (pi-format-web-search-result "Overview of Scheme." grounding))
      ) ;
  (let ((txt (cdr (assoc 'text res))))
    (check-false (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "## Additional Search Results"))
    (check-true (string-contains? txt "1. Scheme Wiki - https://community.schemewiki.org")
    ) ;check-true
    (check-true (string-contains? txt
                  "2. S7 Scheme - https://ccrma.stanford.edu/software/snd/snd/s7.html"
                ) ;string-contains?
    ) ;check-true
  ) ;let
) ;let*

;; 5. 混合：被引用的入 Sources，未引用的入 Additional Search Results（且去重）
(let* ((citations (list (list (cons 'title "Source A") (cons 'url "https://a.test"))))
       (results (list (list (cons 'title "Source A") (cons 'url "https://a.test"))
                  (list (cons 'title "Source B") (cons 'url "https://b.test"))
                  (list (cons 'title "Source B") (cons 'url "https://b.test"))
                ) ;list
       ) ;results
       (grounding (list (cons 'citations citations)
                    (cons 'searchResults results)
                    (cons 'searchQueries '("test query"))
                  ) ;list
       ) ;grounding
       (res (pi-format-web-search-result "Answer text." grounding))
      ) ;
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "[1] Source A - https://a.test"))
    (check-true (string-contains? txt "## Additional Search Results"))
    (check-true (string-contains? txt "1. Source B - https://b.test"))
    ;; 不应在 Additional 中出现已在 Sources 的 Source A
    (check-false (string-contains? txt "2. Source A"))
  ) ;let
) ;let*

;; 6. 原文已含 ## Sources，不重复追加
(let* ((orig "Pre-formatted answer.\n\n## Sources\n[1] Custom - https://custom.org")
       (citations (list (list (cons 'title "Ignore me") (cons 'url "https://ignore.org")))
       ) ;citations
       (grounding (list (cons 'citations citations) (cons 'searchResults '())))
       (res (pi-format-web-search-result orig grounding))
      ) ;
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "[1] Custom - https://custom.org"))
    (check-false (string-contains? txt "[1] Ignore me"))
  ) ;let
) ;let*

;; 7. 长度截断测试
(let* ((long-text (make-string 300 #\x))
       (res (pi-format-web-search-result long-text '() :max-bytes 100))
      ) ;
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "[Truncated]"))
    (check-true (<= (string-length txt) 150))
  ) ;let
) ;let*

;; 8. 支持接收包含 groundingMetadata 的原始 JSON 字典
(let* ((raw-json
         (list (cons "groundingMetadata"
                 (list (cons "webSearchQueries" #("raw query"))
                   (cons "groundingChunks"
                     #(((web (title . "Raw Title") (uri . "https://raw.test"))))
                   ) ;cons
                   (cons "groundingSupports"
                     #(((segment (text . "text") (endIndex . 4))
                        (groundingChunkIndices . #(0)))
                     ) ;#
                   ) ;cons
                 ) ;list
               ) ;cons
         ) ;list
       ) ;raw-json
       (res (pi-format-web-search-result "Answer with raw json" raw-json))
      ) ;
  (let ((txt (cdr (assoc 'text res))))
    (check-true (string-contains? txt "## Sources"))
    (check-true (string-contains? txt "[1] Raw Title - https://raw.test"))
  ) ;let
) ;let*

(check-report)
