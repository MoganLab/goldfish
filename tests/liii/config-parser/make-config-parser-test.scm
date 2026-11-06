;; make-config-parser
;; 创建一个空的 INI 配置解析器。
;;
;; 语法
;; ----
;; (make-config-parser)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; <config-parser>
;; 一个空的配置解析器实例。
;;
;; 说明
;; ----
;; 返回的解析器不包含任何 section 或 option。
;; 使用 `config-read-string` 或 `config-read-file` 填充内容。

(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

(let ((config (make-config-parser)))
  (check (config-parser? config) => #t)
  (check (config-sections config) => '())
) ;let

(check-report)
