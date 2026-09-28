;;
;; Async vs Sync HTTP Comparison Demo
;; 对比展示异步和同步 HTTP 请求的区别
;;

(import (liii http) (liii http-async) (liii sys) (scheme time))

(display "========================================\n")
(display "  Async vs Sync HTTP Comparison Demo\n")
(display "========================================\n\n")

;; 同步请求函数（使用阻塞式 http-get）

(define (sync-requests)
  (display "--- Synchronous Requests ---\n")
  (let ((start (current-second)))
    ;; 第一个请求
    (display "Sending request 1... ")
    (let ((r1 (http-get "https://httpbin.org/delay/1")))
      (display (string-append "Done (" (number->string (r1 'elapsed)) "s)\n"))
    ) ;let
    ;; 第二个请求
    (display "Sending request 2... ")
    (let ((r2 (http-get "https://httpbin.org/delay/1")))
      (display (string-append "Done (" (number->string (r2 'elapsed)) "s)\n"))
    ) ;let
    ;; 第三个请求
    (display "Sending request 3... ")
    (let ((r3 (http-get "https://httpbin.org/delay/1")))
      (display (string-append "Done (" (number->string (r3 'elapsed)) "s)\n"))
    ) ;let
    (let ((total (- (current-second) start)))
      (display (string-append "\nTotal time (sync): " (number->string total) "s\n"))
      total
    ) ;let
  ) ;let
) ;define

;; 异步请求函数

(define (async-requests)
  (display "\n--- Asynchronous Requests ---\n")
  (let ((start (current-second)) (completed 0))
    ;; 启动所有三个请求（不会阻塞）
    (display "Sending all 3 requests concurrently...\n")

    (http-async-get "https://httpbin.org/delay/1"
      (lambda (r)
        (set! completed (+ completed 1))
        (display (string-append "  Request 1 completed (elapsed: "
                   (number->string (r 'elapsed))
                   "s)\n"
                 ) ;string-append
        ) ;display
      ) ;lambda
    ) ;http-async-get

    (http-async-get "https://httpbin.org/delay/1"
      (lambda (r)
        (set! completed (+ completed 1))
        (display (string-append "  Request 2 completed (elapsed: "
                   (number->string (r 'elapsed))
                   "s)\n"
                 ) ;string-append
        ) ;display
      ) ;lambda
    ) ;http-async-get

    (http-async-get "https://httpbin.org/delay/1"
      (lambda (r)
        (set! completed (+ completed 1))
        (display (string-append "  Request 3 completed (elapsed: "
                   (number->string (r 'elapsed))
                   "s)\n"
                 ) ;string-append
        ) ;display
      ) ;lambda
    ) ;http-async-get

    (display
      (string-append "All requests initiated (setup time: "
        (number->string (- (current-second) start))
        "s)\n"
      ) ;string-append
    ) ;display

    ;; 等待所有请求完成
    (display "Waiting for all requests to complete...\n")
    (http-wait-all 30)

    (let ((total (- (current-second) start)))
      (display (string-append "\nTotal time (async): " (number->string total) "s\n"))
      total
    ) ;let
  ) ;let
) ;define

;; 执行对比
(let ((sync-time (sync-requests)) (async-time (async-requests)))

  (display "\n========================================\n")
  (display "              Results\n")
  (display "========================================\n")
  (display (string-append "Synchronous total time:  " (number->string sync-time) "s\n")
  ) ;display
  (display (string-append "Asynchronous total time: " (number->string async-time) "s\n")
  ) ;display
  (display
    (string-append "Speedup: ~"
      (number->string (round (/ sync-time async-time)))
      "x faster\n"
    ) ;string-append
  ) ;display
  (display "\nNote: Each request has a 1 second delay on server side.\n")
  (display "      Sync: 1+1+1 = ~3s, Async: max(1,1,1) = ~1s\n")
  (display "========================================\n")
) ;let
