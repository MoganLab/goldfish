;; pi-retry-config-provider-timeout-ms
;; 读取 <pi-retry-config> 的 provider 级单次请求总超时配置。
;;
;; 语法
;; ----
;; (pi-retry-config-provider-timeout-ms cfg)
;;
;; 参数
;; ----
;; cfg : pi-retry-config?
;; 重试配置 record。
;;
;; 返回值
;; ------
;; integer? | #f
;; 毫秒数，未配置时为 #f。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((r1 (%make-pi-retry-config #t 3 2000 60000 0))
      (r2 (%make-pi-retry-config #t 3 2000 60000 0 45000))
     ) ;
  (check (pi-retry-config-provider-timeout-ms r1) => #f)
  (check (pi-retry-config-provider-timeout-ms r2) => 45000)
) ;let

(check-report)
