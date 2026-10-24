;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code tools web-search deepseek)
  (import (scheme base)
    (liii string)
    (liii list)
    (liii json)
    (liii pi code tools read)
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

    ;; 解析 Search API 响应：每条结果取 title/url/snippet/summary
    ;; （summary 缺失时回退到 snippet）
    (define (deepseek-parse-search-results json-data)
      (let ((results (%json-field json-data "results")))
        (let loop
          ((rem (cond ((vector? results) (vector->list results))
                      ((pair? results) results)
                      (else '())
                ) ;cond
           ) ;rem
           (acc '())
          ) ;
          (if (null? rem)
            (reverse acc)
            (let* ((item (car rem))
                   (title (%json-field item "title"))
                   (url (%json-field item "url"))
                   (snippet (%json-field item "snippet"))
                   (summary (%json-field item "summary"))
                   (snippet-str
                     (if (and (string? snippet) (not (string-null? snippet))) snippet "")
                   ) ;snippet-str
                  ) ;
              (if (or (not (string? url)) (string-null? (string-trim-both url)))
                (loop (cdr rem) acc)
                (loop (cdr rem)
                  (cons
                    (list (cons "title" (if (string? title) title url))
                      (cons "url" url)
                      (cons "snippet" snippet-str)
                      (cons "summary"
                        (if (and (string? summary) (not (string-null? summary))) summary snippet-str)
                      ) ;cons
                    ) ;list
                    acc
                  ) ;cons
                ) ;loop
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
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

    ;; 把 fetch 到的页面拼进正文（content 截断，避免正文爆炸）
    (define (deepseek-append-fetched-page text page . opt-max-chars)
      (let* ((max-chars (if (pair? opt-max-chars) (car opt-max-chars) 2000))
             (title (%json-field page "title"))
             (url (%json-field page "url"))
             (content (%json-field page "content"))
             (content-str (if (string? content) content ""))
             (truncated (if (> (string-length content-str) max-chars)
                          (string-append (substring content-str 0 max-chars) "\n[...truncated]")
                          content-str
                        ) ;if
             ) ;truncated
             (section
               (string-append (if (string-null? (string-trim-both text))
                                "## Fetched Pages"
                                (string-append text "\n\n" "## Fetched Pages")
                              ) ;if
                 "\n\n### "
                 (if (string? title) title (if (string? url) url "page"))
                 "\n"
                 (if (string? url) (string-append url "\n") "")
                 truncated
               ) ;string-append
             ) ;section
            ) ;
        section
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
