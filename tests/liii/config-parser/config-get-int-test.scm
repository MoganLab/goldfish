(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-get-int
;; 从配置解析器中获取指定 option 的整数值。
;;
;; 语法
;; ----
;; (config-get-int config section option)
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
;; number
;; option 对应的整数值。
;;
;; 说明
;; ----
;; 内部调用 `config-get` 获取字符串后使用 `string->number` 转换。
;; 支持 DEFAULT section 继承。

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
  (config-read-string config test-ini)
  (check (config-get-int config "topsecret.server.example" "port") => 50022)
  (check (config-get-int config "DEFAULT" "serveraliveinterval") => 45)
) ;let

(check-report)
