(import (liii check)
        (liii http)
        (liii string)
        (liii os)
        (liii rich-json)
        (only (liii lang) display*)
        (liii time)
) ;import

(check-set-mode! 'report-failed)

(let ((env (getenv "GOLDFISH_TEST_HTTP")))
  (when (not env) (exit 0))
) ;let

(let ((r (http-head "https://httpbin.org")))
  (check (r 'status-code) => 200)
  (check (r 'url) => "https://httpbin.org/")
  (check-true (real? (r 'elapsed)))
  ;; NOTE: httpbin.org's LB routes to different backends.
  ;;       Some return "OK", others empty string for reason.
  ;;       HTTP/2+ allows omitting reason phrases.
  (check-true (or (equal? (r 'reason) "OK")
                  (equal? (r 'reason) ""))
  ) ;check-true
  (check (r 'text) => "")
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
  (check ((r 'headers) "content-length") => "9593")
  (check-true (http-ok? r))
) ;let

(let ((r (http-get "https://httpbin.org")))
  (check (r 'status-code) => 200)
  (check-true (> (string-length (r 'text)) 0))
  (check ((r 'headers) "content-type") => "text/html; charset=utf-8")
) ;let

(let ((r (http-get "https://httpbin.org/get"
                  :params '(("key1" . "value1") ("key2" . "value2")))))
      (check-true (string-contains (r 'text) "value1"))
      (check-true (string-contains (r 'text) "value2"))
      (check (r 'url) => "https://httpbin.org/get?key1=value1&key2=value2")
) ;let

(let ((r (http-post "https://httpbin.org/post"
                  :params '(("key1" . "value1") ("key2" . "value2")))))
      (check-true (string-contains (r 'text) "value1"))
      (check-true (string-contains (r 'text) "value2"))
      (check (r 'status-code) => 200)
      (check (r 'url) => "https://httpbin.org/post?key1=value1&key2=value2")
) ;let

(let* ((r (http-post "https://httpbin.org/post"
            :data "This is raw data"))
       (json (string->json (r 'text))))
  (display* (r 'text) "\n")
  (display* json "\n")
  (display* (json->string json) "\n")
  (check (r 'status-code) => 200)
  (check (json-ref json "data") => "This is raw data")
) ;let*

;; Streaming HTTP tests

;; Test streaming GET with simple endpoint
(let ((collected '())
      (userdata-received #f)
      (userdata-expected '("streaming" test "param")))
  (http-stream-get "https://httpbin.org/get"
                   (lambda (chunk userdata)
                     (display userdata)
                     (newline)
                     (set! userdata-received userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   userdata-expected
                   '(("query" . "test_values") ("limit" . "10"))
  ) ;http-stream-get
  (check-true (> (length collected) 0))
  (check userdata-received => userdata-expected)
) ;let

;; Test streaming GET with JSON endpoint
(let ((collected '()))
  (http-stream-get "https://jsonplaceholder.typicode.com/posts/1"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
  ) ;http-stream-get
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "userId"))
  ) ;let
) ;let

;; Test streaming POST with JSON data
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   '()
                   '(("param1" . "value1"))
                   "{\"test\": \"streaming-json\"}"
                   '(("Content-Type" . "application/json"))
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "streaming-json"))
  ) ;let
) ;let

;; Test streaming POST with plain text
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   '()
                   '()
                   "Simple streaming POST test"
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "Simple streaming POST test"))
  ) ;let
) ;let

;; Test streaming POST with XML data
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   '()
                   '()
                   "<root><message>stream-xml-test</message></root>"
                   '(("Content-Type" . "application/xml"))
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "stream-xml-test"))
  ) ;let
) ;let

;; Test streaming POST with form data
(let ((collected '()))
  (http-stream-post "https://httpbin.org/post"
                   (lambda (chunk userdata)
                     (when (> (string-length chunk) 0)
                       (set! collected (cons chunk collected))
                     ) ;when
                   ) ;lambda
                   '()
                   '()
                   "field1=stream-test&field2=form-data"
                   '(("Content-Type" . "application/x-www-form-urlencoded"))
  ) ;http-stream-post
  (let ((response (string-join (reverse collected) "")))
    (check-true (> (string-length response) 0))
    (check-true (string-contains response "stream-test"))
  ) ;let
) ;let

;; Async HTTP tests

;; Test async GET request
(let ((async-completed #f)
      (async-response #f))
  (http-async-get "https://httpbin.org/get"
    (lambda (response)
      (set! async-completed #t)
      (set! async-response response)
    ) ;lambda
  ) ;http-async-get
  (http-wait-all 30)
  (check-true async-completed)
  (check (async-response 'status-code) => 200)
  (check-true (string-contains (async-response 'text) "httpbin.org"))
) ;let

;; Test async POST request
(let ((post-completed #f)
      (post-response #f))
  (http-async-post "https://httpbin.org/post"
    (lambda (response)
      (set! post-completed #t)
      (set! post-response response)
    ) ;lambda
    '()                                    ; params
    "{\"test\": \"async-post\"}"              ; body
    '(("Content-Type" . "application/json")) ; headers
    '()                                   ; proxy
  ) ;http-async-post
  (http-wait-all 30)
  (check-true post-completed)
  (check (post-response 'status-code) => 200)
  (check-true (string-contains (post-response 'text) "async-post"))
) ;let

;; Test multiple concurrent async requests
(let ((completed-count 0)
      (start-time (current-second)))
  (http-async-get "https://httpbin.org/delay/1" (lambda (r) (set! completed-count (+ completed-count 1))))
  (http-async-get "https://httpbin.org/delay/1" (lambda (r) (set! completed-count (+ completed-count 1))))
  (http-async-get "https://httpbin.org/delay/1" (lambda (r) (set! completed-count (+ completed-count 1))))
  (http-wait-all 30)
  (let ((elapsed (- (current-second) start-time)))
    ;; All 3 requests should complete in ~1s (concurrent), not ~3s (sequential)
    (check completed-count => 3)
    ;; Allow some tolerance for network latency
    (check-true (< elapsed 5.0)) ; Async requests should complete concurrently
  ) ;let
) ;let

;; Test http-poll returns correct count
(let ((poll-count 0))
  (http-async-get "https://httpbin.org/get" (lambda (r) (set! poll-count (+ poll-count 1))))
  ;; Poll until completion
  (let loop ((pending #t))
    (when pending
      (let ((executed (http-poll)))
        (if (> executed 0)
            (display (string-append "Poll executed " (number->string executed) " callback(s)\n"))
            (begin
              (sleep 0.05)
              (loop #t)
            ) ;begin
        ) ;if
      ) ;let
    ) ;when
  ) ;let
  (check poll-count => 1)
) ;let

(check-report)

