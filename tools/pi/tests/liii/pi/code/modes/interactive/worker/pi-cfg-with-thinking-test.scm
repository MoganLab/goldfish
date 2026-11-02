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
  (liii pi code modes interactive worker))

(check-set-mode! 'report-failed)

;; 覆盖已有项 / cfg 无该项时前置追加
(check (pi-cfg-with-thinking '((thinking-level . "off") (model-id . "m")) "high")
  => '((thinking-level . "high") (model-id . "m")))
(check (pi-cfg-with-thinking '((model-id . "m")) "low")
  => '((thinking-level . "low") (model-id . "m")))

(check-report)
