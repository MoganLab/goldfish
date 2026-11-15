;; pi-cfg-with-thinking
;; pi-cfg-with-thinking 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (pi-cfg-with-thinking ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base)
  (liii check)
  (liii pi code config)
  (liii pi code modes interactive worker)
) ;import

(check-set-mode! 'report-failed)

;; 覆盖 thinking-level，同时保留其余配置字段
(let* ((cfg (make-pi-config :thinking-level "off" :model-id "m"))
       (out (pi-cfg-with-thinking cfg "high"))
      ) ;
  (check-true (pi-config? out))
  (check (pi-cfg-ref out 'thinking-level) => "high")
  (check (pi-cfg-ref out 'model-id) => "m")
  (check (pi-cfg-ref cfg 'thinking-level) => "off")
) ;let*

(let* ((cfg (make-pi-config :model-id "m"))
       (out (pi-cfg-with-thinking cfg "low"))
      ) ;
  (check (pi-cfg-ref out 'thinking-level) => "low")
  (check (pi-cfg-ref out 'model-id) => "m")
) ;let*

(check-report)
