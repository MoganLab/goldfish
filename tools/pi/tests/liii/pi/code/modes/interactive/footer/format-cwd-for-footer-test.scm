;; format-cwd-for-footer
;; format-cwd-for-footer 的相关操作与逻辑实现。
;;
;; 语法
;; ----
;; (format-cwd-for-footer ...)
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
  (liii pi code modes interactive footer))

(check-set-mode! 'report)

(check (format-cwd-for-footer "/home/da" "/home/da") => "~")
(check (format-cwd-for-footer "/home/da/git/goldfish" "/home/da") => "~/git/goldfish")
(check (format-cwd-for-footer "/tmp/test" "/home/da") => "/tmp/test")
(check (format-cwd-for-footer "/home/da" #f) => "/home/da")

(check-report)
