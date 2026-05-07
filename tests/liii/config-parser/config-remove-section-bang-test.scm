(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-remove-section!
;; 删除配置解析器中指定的 section 及其所有 option。
;;
;; 语法
;; ----
;; (config-remove-section! config section)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; section : string
;; 要删除的 section 名称。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 错误处理
;; ----
;; config-error
;; 不能删除 DEFAULT section；section 不存在时抛出。

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\nport=5432\n")
  (config-remove-section! config "database")
  (check (config-has-section? config "database") => #f)
  (check (config-sections config) => '())
) ;let

(check-report)
