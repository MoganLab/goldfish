;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi web-search deepseek)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii json)
    (liii pi code core tools read)
  ) ;import
  (export deepseek-build-search-request-body deepseek-build-fetch-request-body
    deepseek-build-search-endpoint deepseek-build-fetch-endpoint
    deepseek-parse-search-results deepseek-sources-from-results
    deepseek-format-search-text deepseek-parse-fetch-page
    deepseek-append-fetched-page
  ) ;export
  (begin

    (define (%json-field obj key)
      (if (or (not (pair? obj)) (equal? obj '(())))
        #f
        (let ((p
                (or (assoc key obj)
                  (if (symbol? key) (assoc (symbol->string key) obj) #f)
                  (if (string? key) (assoc (string->symbol key) obj) #f)
                ) ;or
              ) ;p
             ) ;
          (if (pair? p) (cdr p) #f)
        ) ;let
      ) ;if
    ) ;define

    ;; DeepSeek Search API 的请求体：POST /v1/tools/search
    (define (deepseek-build-search-request-body query . opt-count)
      (let ((count (if (pair? opt-count) (car opt-count) 10)))
        (list (cons "query" query) (cons "count" count))
      ) ;let
    ) ;define

    ;; DeepSeek Fetch API 的请求体：POST /v1/tools/fetch
    (define (deepseek-build-fetch-request-body url)
      (list (cons "url" url))
    ) ;define

    (define (%strip-trailing-slash url)
      (if
        (and (string? url)
          (> (string-length url) 0)
          (char=? (string-ref url (- (string-length url) 1)) #\/)
        ) ;and
        (substring url 0 (- (string-length url) 1))
        url
      ) ;if
    ) ;define

    (define (deepseek-build-search-endpoint base-url)
      (string-append (%strip-trailing-slash base-url) "/v1/tools/search")
    ) ;define

    (define (deepseek-build-fetch-endpoint base-url)
      (string-append (%strip-trailing-slash base-url) "/v1/tools/fetch")
    ) ;define

    (define (%results-list json-data)
      (let ((results (%json-field json-data "results")))
        (cond ((vector? results) (vector->list results))
              ((pair? results) results)
              (else '())
        ) ;cond
      ) ;let
    ) ;define

    ;; 单条结果 -> (("title" . ...) ("url" . ...) ("snippet" . ...) ("summary" . ...))
    ;; summary 缺失时回退到 snippet；无有效 url 时返回 #f（跳过该条）
    (define (%result-item item)
      (let* ((title (%json-field item "title"))
             (url (%json-field item "url"))
             (snippet (%json-field item "snippet"))
             (summary (%json-field item "summary"))
             (snippet-str
               (if (and (string? snippet) (not (string-null? snippet))) snippet "")
             ) ;snippet-str
             (summary-str
               (if (and (string? summary) (not (string-null? summary))) summary snippet-str)
             ) ;summary-str
            ) ;
        (and (string? url)
          (not (string-null? (string-trim-both url)))
          (list (cons "title" (if (string? title) title url))
            (cons "url" url)
            (cons "snippet" snippet-str)
            (cons "summary" summary-str)
          ) ;list
        ) ;and
      ) ;let*
    ) ;define

    ;; 解析 Search API 响应：每条结果取 title/url/snippet/summary
    (define (deepseek-parse-search-results json-data)
      (let loop
        ((rem (%results-list json-data)) (acc '()))
        (if (null? rem)
          (reverse acc)
          (let ((parsed (%result-item (car rem))))
            (loop (cdr rem) (if parsed (cons parsed acc) acc))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; results -> ((title . url) ...)，供 pi-format-web-search-result 的 :sources
    (define (deepseek-sources-from-results results)
      (map
        (lambda (item)
          (let ((title (%json-field item "title")) (url (%json-field item "url")))
            (list (cons 'title (if (string? title) title url)) (cons 'url url))
          ) ;let
        ) ;lambda
        (if (vector? results) (vector->list results) results)
      ) ;map
    ) ;define

    ;; 正文：每条结果 "[n] title\nsummary"
    (define (deepseek-format-search-text results)
      (let loop
        ((rem results) (i 1) (acc '()))
        (if (null? rem)
          (string-join (reverse acc) "\n\n")
          (let* ((item (car rem))
                 (title (%json-field item "title"))
                 (summary (%json-field item "summary"))
                 (line (string-append "["
                         (number->string i)
                         "] "
                         (if (string? title) title "")
                         "\n"
                         (if (string? summary) summary "")
                       ) ;string-append
                 ) ;line
                ) ;
            (loop (cdr rem) (+ i 1) (cons line acc))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 解析 Fetch API 响应：((title . ...) (content . ...))
    (define (deepseek-parse-fetch-page json-data)
      (let ((title (%json-field json-data "title"))
            (content (%json-field json-data "content"))
           ) ;
        (list (cons "title" (if (string? title) title ""))
          (cons "content" (if (string? content) content ""))
        ) ;list
      ) ;let
    ) ;define

    (define (%truncate-str str max-chars)
      (if (> (string-length str) max-chars)
        (string-append (substring str 0 max-chars) "\n[...truncated]")
        str
      ) ;if
    ) ;define

    (define (%page-title-str page)
      (let ((title (%json-field page "title")) (url (%json-field page "url")))
        (cond ((string? title) title)
              ((string? url) url)
              (else "page")
        ) ;cond
      ) ;let
    ) ;define

    ;; 把 fetch 到的页面拼进正文（content 截断，避免正文爆炸）
    (define (deepseek-append-fetched-page text page . opt-max-chars)
      (let* ((max-chars (if (pair? opt-max-chars) (car opt-max-chars) 2000))
             (url (%json-field page "url"))
             (content (%json-field page "content"))
             (content-str (if (string? content) content ""))
             (head (if (string-null? (string-trim-both text))
                     "## Fetched Pages"
                     (string-append text "\n\n" "## Fetched Pages")
                   ) ;if
             ) ;head
            ) ;
        (string-append head
          "\n\n### "
          (%page-title-str page)
          "\n"
          (if (string? url) (string-append url "\n") "")
          (%truncate-str content-str max-chars)
        ) ;string-append
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
