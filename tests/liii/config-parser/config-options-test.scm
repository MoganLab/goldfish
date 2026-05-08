(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-options
;; 获取指定 section 下所有 option 名（含 DEFAULT 继承）。
;;
;; 语法
;; ----
;; (config-options config section)
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
;; list of string
;; option 名称列表，包含从 DEFAULT section 继承的 option。
;;
;; 错误处理
;; ----
;; config-error
;; section 不存在时抛出。

(define test-ini
  (&- #""
    [DEFAULT]
    ServerAliveInterval = 45
    Compression = yes
    ForwardX11 = yes

    [forge.example]
    User = hg
    ""
  ) ;&-
) ;define

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\nport=5432\n")
  (check (length (config-options config "database")) => 2)
  (check (config-has-option? config "database" "host") => #t)
  (check (config-has-option? config "database" "port") => #t)
) ;let

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  ;; forge.example 有 User，DEFAULT 有 ServerAliveInterval/Compression/ForwardX11
  (check (config-has-option? config "forge.example" "user") => #t)
  (check (config-has-option? config "forge.example" "compression") => #t)
) ;let

(check-report)
