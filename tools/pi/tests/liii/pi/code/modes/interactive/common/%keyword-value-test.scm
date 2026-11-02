;; %keyword-value
;; %keyword-value 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (%keyword-value ...)
;;
;; 参数
;; ----
;; 详见语法说明。
;;
;; 返回值
;; ----
;; any
;; 视具体调用结果而定。

(import (scheme base) (liii check) (liii pi code modes interactive common))

(check-set-mode! 'report)

(check (%keyword-value :key '(:key "val") #f) => "val")
(check (%keyword-value :missing '(:key "val") "default") => "default")
(check (%keyword-value :key '() "default") => "default")

(check-report)
