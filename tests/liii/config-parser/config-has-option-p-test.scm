(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-has-option?
;; 检查配置解析器中是否存在指定的 option。
;;
;; 语法
;; ----
;; (config-has-option? config section option)
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
;; option 名称（大小写不敏感）。
;;
;; 返回值
;; ----
;; boolean
;; option 存在时返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; 如果当前 section 没有该 option，会在 DEFAULT section 中查找（继承机制）。

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
  (config-read-string config "[database]\nhost=localhost\n")
  (check (config-has-option? config "database" "host") => #t)
  (check (config-has-option? config "database" "missing") => #f)
) ;let

;; 包含 DEFAULT 继承的 option
(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  (check (config-has-option? config "forge.example" "user") => #t)
  (check (config-has-option? config "forge.example" "compression") => #t)
) ;let

(check-report)
