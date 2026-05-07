(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-remove-option!
;; 删除指定 section 中的 option。
;;
;; 语法
;; ----
;; (config-remove-option! config section option)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; section : string
;; section 名称。
;;
;; option : string
;; 要删除的 option 名称。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 错误处理
;; ----
;; config-error
;; section 不存在、option 不存在、或 option 来自 DEFAULT 继承时抛出。

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\nport=5432\n")
  (config-remove-option! config "database" "host")
  (check (config-has-option? config "database" "host") => #f)
  (check (config-has-option? config "database" "port") => #t)
) ;let

(check-report)
