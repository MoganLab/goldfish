;; pi-request-api-key
;; 获取本次请求使用的 API 密钥（优先委托 request-key 钩子，回退单 key）。
;;
;; 语法
;; ----
;; (pi-request-api-key config)
;;
;; 参数
;; ----
;; config : alist?
;; 请求配置关联表。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 本次请求使用的 API 密钥，若未配置则返回 #f。

(import (liii check) (liii pi ai keyring))

(check-set-mode! 'report-failed)

;; 无钩子时回退 config 的 api-key
(check (pi-request-api-key (list (cons 'api-key "auth-json-key")))
  =>
  "auth-json-key"
) ;check
(check (pi-request-api-key '()) => #f)

;; request-key 钩子：每次请求委托钩子选取 key
(let* ((calls 0)
       (cfg
         (list (cons 'api-key "fallback")
           (cons 'request-key
             (lambda (config)
               (set! calls (+ calls 1))
               (string-append "pool-key-" (number->string calls))
             ) ;lambda
           ) ;cons
         ) ;list
       ) ;cfg
      ) ;
  (check (pi-request-api-key cfg) => "pool-key-1")
  (check (pi-request-api-key cfg) => "pool-key-2")
  (check calls => 2)
) ;let*

;; 钩子为非过程值时视为无钩子
(check (pi-request-api-key (list (cons 'api-key "k") (cons 'request-key #f)))
  =>
  "k"
) ;check

(check-report)
