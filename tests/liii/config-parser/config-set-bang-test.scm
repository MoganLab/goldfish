(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-set!
;; 在指定 section 中设置 option 的值。
;;
;; 语法
;; ----
;; (config-set! config section option value)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; section : string
;; section 名称（必须已存在）。
;;
;; option : string
;; option 名称（大小写不敏感，存储为小写）。
;;
;; value : string
;; 要设置的值。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 错误处理
;; ----
;; config-error
;; section 不存在时抛出。

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\nport=5432\n")
  ;; 修改已有 option
  (config-set! config "database" "host" "127.0.0.1")
  (check (config-get config "database" "host") => "127.0.0.1")
) ;let

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\n")
  ;; 添加新 option
  (config-set! config "database" "timeout" "30")
  (check (config-get config "database" "timeout") => "30")
) ;let

(check-report)
