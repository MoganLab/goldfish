(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-sections
;; 返回配置解析器中所有 section 名列表（不含 DEFAULT）。
;;
;; 语法
;; ----
;; (config-sections config)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; 返回值
;; ----
;; list of string
;; 所有 section 名称列表，不包含 DEFAULT section。

(define test-ini
  (&- #""
    [DEFAULT]
    ServerAliveInterval = 45
    Compression = yes
    ForwardX11 = yes

    [forge.example]
    User = hg

    [topsecret.server.example]
    Port = 50022
    ForwardX11 = no
    ""
  ) ;&-
) ;define

(let ((config (make-config-parser)))
  (check (config-sections config) => '())
) ;let

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  (check (config-sections config)
    =>
    '("forge.example" "topsecret.server.example")
  ) ;check
) ;let

(check-report)
