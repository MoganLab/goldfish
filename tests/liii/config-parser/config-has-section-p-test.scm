(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-has-section?
;; 检查配置解析器中是否存在指定的 section。
;;
;; 语法
;; ----
;; (config-has-section? config section)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; section : string
;; section 名称。
;;
;; 返回值
;; ----
;; boolean
;; section 存在时返回 #t，否则返回 #f。

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\nport=5432\n")
  (check (config-has-section? config "database") => #t)
  (check (config-has-section? config "missing") => #f)
) ;let

(check-report)
