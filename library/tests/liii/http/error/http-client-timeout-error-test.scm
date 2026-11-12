;; http-client-timeout-error
;; 抛出客户端 HTTP 请求超时异常。
;;
;; 语法
;; ----
;; (http-client-timeout-error . args)
;;
;; 参数
;; ----
;; args : any ...
;;   附加的错误信息。
;;
;; 返回值
;; ----
;; 无（抛出异常）
;;
;; 描述
;; ----
;; 当客户端 HTTP 请求（如 http-head、http-get、http-post、http-delete）
;; 在设置的 timeout 期限内未能完成时抛出此错误。

(import (liii check) (liii http error) (liii http))

(check-set-mode! 'report-failed)

;; 基础抛错与捕获测试
(check-catch 'http-client-timeout-error
  (http-client-timeout-error "Request timed out")
) ;check-catch

(check-catch 'http-client-timeout-error
  (http-client-timeout-error)
) ;check-catch

;; 从 (liii http) 也能捕获 HTTP 客户端超时异常
(check-catch 'http-client-timeout-error
  (http-get "http://192.0.2.1" :timeout 0.2)
) ;check-catch

(check-catch 'http-client-timeout-error
  (http-post "http://192.0.2.1" :data "test" :timeout 0.2)
) ;check-catch

(check-catch 'http-client-timeout-error
  (http-head "http://192.0.2.1" :timeout 0.2)
) ;check-catch

(check-catch 'http-client-timeout-error
  (http-delete "http://192.0.2.1" :timeout 0.2)
) ;check-catch

(check-report)
