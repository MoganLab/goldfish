(import (liii check) (liii pi ai keyring))

(check-set-mode! 'report-failed)

;; pi-request-api-key / pi-key-rotation? / pi-rotate-api-key!
;; 核心通用 key 钩子：无钩子时回退 api-key 单 key 路径；
;; 扩展注入 request-key / rotate-key 钩子后委托其策略。

;; 1. 无钩子：request 回退 config 的 api-key
(check (pi-request-api-key (list (cons 'api-key "auth-json-key"))) => "auth-json-key")
(check (pi-request-api-key '()) => #f)

;; 2. 无钩子：rotation 判定为 #f，rotate 返回 #f（走既有重试/报错路径）
(check-true (not (pi-key-rotation? (list (cons 'api-key "k")))))
(check (pi-rotate-api-key! (list (cons 'api-key "k")) "k" 429) => #f)

;; 3. request-key 钩子：每次请求委托钩子选取 key
(let* ((calls 0)
       (cfg (list (cons 'api-key "fallback")
                  (cons 'request-key
                    (lambda (config)
                      (set! calls (+ calls 1))
                      (string-append "pool-key-" (number->string calls))
                    ) ;lambda
                  ) ;cons
           ) ;list
       ) ;let*
      ) ;
  (check (pi-request-api-key cfg) => "pool-key-1")
  (check (pi-request-api-key cfg) => "pool-key-2")
  (check calls => 2)
) ;let*

;; 4. rotate-key 钩子：rotation 判定为 #t，429/403 委托钩子换 key
(let ((rotated '()))
  (let* ((cfg (list (cons 'api-key "k1")
                    (cons 'rotate-key
                      (lambda (config current-key status)
                        (set! rotated (list current-key status))
                        "k2"
                      ) ;lambda
                    ) ;cons
                ) ;list
          ) ;let*
        ) ;
    (check-true (pi-key-rotation? cfg))
    (check (pi-rotate-api-key! cfg "k1" 429) => "k2")
    (check rotated => '("k1" 429))
  ) ;let
) ;let

;; 5. 钩子为非过程值时视为无钩子
(check (pi-request-api-key (list (cons 'api-key "k") (cons 'request-key #f))) => "k")
(check-true (not (pi-key-rotation? (list (cons 'rotate-key "nope")))))

(check-report)
