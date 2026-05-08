(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-add-section!
;; 添加新的 section 到配置解析器。
;;
;; 语法
;; ----
;; (config-add-section! config section)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; section : string
;; 新 section 的名称。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 错误处理
;; ----
;; config-error
;; section 已存在时抛出。

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\n")
  (config-add-section! config "server")
  (check (config-has-section? config "server") => #t)
  (config-set! config "server" "name" "web")
  (check (config-get config "server" "name") => "web")
) ;let

(check-report)
