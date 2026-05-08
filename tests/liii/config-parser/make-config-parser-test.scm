(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

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

;; config-parser?
;; 判断值是否为 <config-parser> 类型。
;;
;; 语法
;; ----
;; (config-parser? obj)
;;
;; 参数
;; ----
;; obj : any
;; 待检测的值。
;;
;; 返回值
;; ----
;; boolean
;; 当 obj 为配置解析器时返回 #t，否则返回 #f。

(let ((config (make-config-parser)))
  (check (config-parser? config) => #t)
  (check (config-sections config) => '())
) ;let

(check (config-parser? 42) => #f)
(check (config-parser? "string") => #f)

(check-report)
