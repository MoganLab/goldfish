;; pi-resolve-stream-idle-timeout
;; 解析流式请求的空闲超时（秒），支持数字、0 / "disabled" 关闭与 settings 回退。
;;
;; 语法
;; ----
;; (pi-resolve-stream-idle-timeout settings timeout idle-timeout)
;;
;; 参数
;; ----
;; settings : pi-settings? | list?
;; 全局设置。
;; timeout : number? | #f
;; 位置兼容超时（秒）。
;; idle-timeout : number? | string? | symbol? | #f
;; 显式空闲超时。
;;
;; 返回值
;; ------
;; number? | #f
;; 空闲超时秒数，关闭时为 #f。

(import (liii check) (liii pi ai config record) (liii pi ai retry))

(check-set-mode! 'report-failed)

(check (pi-resolve-stream-idle-timeout '() #f 5) => 5)
(check (pi-resolve-stream-idle-timeout '() #f 0) => #f)
(check (pi-resolve-stream-idle-timeout '() #f "disabled") => #f)
(check (pi-resolve-stream-idle-timeout '() 12 #f) => 12)
(check (pi-resolve-stream-idle-timeout (pi-settings :http-idle-timeout-ms 60000) #f #f)
  =>
  60.0
) ;check
(check (pi-resolve-stream-idle-timeout (pi-settings :http-idle-timeout-ms "disabled")
         #f
         #f
       ) ;pi-resolve-stream-idle-timeout
  =>
  #f
) ;check

(check-report)
