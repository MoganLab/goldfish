;; pi-settings-http-idle-timeout-ms
;; 读取 <pi-settings> 的 HTTP 空闲超时配置。
;;
;; 语法
;; ----
;; (pi-settings-http-idle-timeout-ms settings)
;;
;; 参数
;; ----
;; settings : pi-settings?
;; 全局设置 record。
;;
;; 返回值
;; ------
;; integer? | string?
;; 毫秒数或 "disabled"。

(import (liii check) (liii pi ai config types))

(check-set-mode! 'report-failed)

(let ((s1 (%make-pi-settings "google" "m" #f #f (%make-pi-retry-config #t 3 2000 60000 #f) #f #f #f '() #t))
      (s2 (%make-pi-settings "google" "m" #f #f (%make-pi-retry-config #t 3 2000 60000 #f) #f #f #f '() #t 120000))
     ) ;
  (check (pi-settings-http-idle-timeout-ms s1) => 300000)
  (check (pi-settings-http-idle-timeout-ms s2) => 120000)
) ;let

(check-report)
