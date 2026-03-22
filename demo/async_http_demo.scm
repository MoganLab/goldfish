;;
;; Async HTTP Demo for Goldfish Scheme
;; 演示 libcpr 异步 HTTP 绑定的使用
;;

(import (liii http)
        (liii sys)
        (liii time)
) ;import

(display "=== Goldfish Scheme Async HTTP Demo ===\n\n")

;; ---------------------------------------------------------
;; Demo 1: 基本异步 GET 请求
;; ---------------------------------------------------------
(display "Demo 1: Basic Async GET\n")
(display "Starting async GET to httpbin.org...\n")

(define start-time (current-second))
(define callback-executed #f)

;; 发起异步 GET 请求，callback 会在请求完成后被调用
(http-async-get "https://httpbin.org/get"
  (lambda (response)
    (set! callback-executed #t)
    (display "\n  [Callback] Async GET completed!\n")
    (display (string-append "  Status: " (number->string (response 'status-code)) "\n"))
    (display (string-append "  URL: " (response 'url) "\n"))
    (display (string-append "  Elapsed: " (number->string (response 'elapsed)) " seconds\n"))
  ) ;lambda
) ;http-async-get

;; 请求立即返回，不阻塞
(display (string-append "Request initiated immediately (elapsed: " 
                        (number->string (- (current-second) start-time))
                        "s)\n")
) ;display

;; 等待请求完成（这会阻塞直到所有异步请求完成）
(display "Waiting for async request to complete...\n")
(http-wait-all 10)  ; 最多等待 10 秒
(display (string-append "Total elapsed time: " 
                        (number->string (- (current-second) start-time))
                        "s\n\n")
) ;display

;; ---------------------------------------------------------
;; Demo 2: 多个并发异步请求
;; ---------------------------------------------------------
(display "Demo 2: Multiple Concurrent Async Requests\n")
(display "Starting 3 concurrent requests...\n")

(define concurrent-start (current-second))
(define completed-count 0)
(define urls '("https://httpbin.org/delay/1"
               "https://httpbin.org/delay/1"
               "https://httpbin.org/delay/1")
) ;define

(for-each
  (lambda (url)
    (http-async-get url
      (lambda (response)
        (set! completed-count (+ completed-count 1))
        (display (string-append "  [Callback #" (number->string completed-count) 
                                "] Completed: " (response 'url) "\n")
        ) ;display
      ) ;lambda
    ) ;http-async-get
  ) ;lambda
  urls
) ;for-each

;; 所有请求立即返回（不等待）
(display (string-append "All 3 requests initiated (elapsed: "
                        (number->string (- (current-second) concurrent-start))
                        "s)\n")
) ;display

;; 等待所有请求完成
(display "Waiting for all requests to complete...\n")
(http-wait-all 30)  ; 最多等待 30 秒

(let ((total-time (- (current-second) concurrent-start)))
  (display (string-append "All 3 requests completed in: " 
                          (number->string total-time) "s\n")
  ) ;display
  (if (< total-time 2.5)
      (display "  -> Requests were executed concurrently! (sequential would take ~3s)\n\n")
      (display "  -> Note: Network latency may vary\n\n")
  ) ;if
) ;let

;; ---------------------------------------------------------
;; Demo 3: 使用 http-poll 非阻塞检查
;; ---------------------------------------------------------
(display "Demo 3: Non-blocking http-poll\n")
(display "Starting async request with manual polling...\n")

(define poll-start (current-second))
(define poll-count 0)

(http-async-get "https://httpbin.org/get"
  (lambda (response)
    (display (string-append "  [Callback] Request completed after "
                            (number->string poll-count)
                            " polls\n")
    ) ;display
  ) ;lambda
) ;http-async-get

;; 使用 http-poll 非阻塞检查
(let loop ((pending #t))
  (when pending
    (set! poll-count (+ poll-count 1))
    (let ((executed (http-poll)))
      (if (> executed 0)
          (display (string-append "  Poll #" (number->string poll-count) 
                                  ": callback executed\n")
          ) ;display
          (begin
            (display (string-append "  Poll #" (number->string poll-count) 
                                    ": no completion yet\n")
            ) ;display
            (sleep 0.1)  ; 等待 100ms
            (loop #t)
          ) ;begin
      ) ;if
    ) ;let
  ) ;when
) ;let

(display (string-append "Completed using polling in: "
                        (number->string (- (current-second) poll-start))
                        "s\n\n")
) ;display

;; ---------------------------------------------------------
;; Demo 4: 异步 POST 请求
;; ---------------------------------------------------------
(display "Demo 4: Async POST Request\n")

(http-async-post "https://httpbin.org/post"
  (lambda (response)
    (display "  [Callback] POST request completed!\n")
    (display (string-append "  Status: " (number->string (response 'status-code)) "\n"))
  ) ;lambda
  '()                                    ; params
  "{\"message\": \"Hello from Goldfish Scheme\"}"  ; body
  '(("Content-Type" . "application/json"))   ; headers
  '()                                   ; proxy
) ;http-async-post

(http-wait-all 10)
(display "POST demo completed.\n\n")

;; ---------------------------------------------------------
;; Summary
;; ---------------------------------------------------------
(display "=== Demo Summary ===\n")
(display "The async HTTP API provides:\n")
(display "  1. http-async-get: Non-blocking GET requests\n")
(display "  2. http-async-post: Non-blocking POST requests\n")
(display "  3. http-poll: Check for completed requests without blocking\n")
(display "  4. http-wait-all: Block until all requests complete\n")
(display "\nKey benefits:\n")
(display "  - Multiple requests can execute concurrently\n")
(display "  - Main thread is not blocked during network I/O\n")
(display "  - Callbacks are executed in the main thread (S7-safe)\n")
(display "  - Uses libcpr's internal thread pool for true async I/O\n")

;  ---

(display "=== 极端并发测试 ===\n")
(display "同时发起 10 个请求，每个服务器端延迟 2 秒\n\n")

(let ((start (current-second))
      (completed 0)
      (n 10))
  
  ;; 启动所有请求
  (display "Launching requests... ")
  (do ((i 0 (+ i 1)))
      ((>= i n))
    (http-async-get (string-append "https://httpbin.org/delay/2?req=" (number->string i))
      (lambda (r)
        (set! completed (+ completed 1))
        (display (string-append "#" (number->string completed) " "))
      ) ;lambda
    ) ;http-async-get
  ) ;do
  
  (display "Done!\n")
  (display (string-append "Setup time: " 
                          (number->string (- (current-second) start))
                          "s\n\n")
  ) ;display
  
  ;; 等待完成
  (display "Waiting... ")
  (http-wait-all 60)
  
  (let ((total (- (current-second) start)))
    (display "\n\n=== 结果 ===\n")
    (display (string-append "Completed: " (number->string completed) "/" (number->string n) "\n"))
    (display (string-append "Total time: " (number->string total) "s\n"))
    
    (if (< total 10)
        (begin
          (display "\n✓ 验证通过：真正的异步并发！\n")
          (display (string-append "  并发度: ~" (number->string (round (/ (* n 2) total))) "x\n"))
        ) ;begin
        (begin
          (display "\n✗ 可能是同步执行\n")
          (display (string-append "  预期: <5s, 实际: " (number->string total) "s\n"))
        ) ;begin
    ) ;if
  ) ;let
) ;let
