(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-get
;; 从配置解析器中获取指定 section 和 option 的值（字符串）。
;;
;; 语法
;; ----
;; (config-get config section option)
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
;; string
;; option 对应的值。
;;
;; 说明
;; ----
;; 如果当前 section 没有该 option，会从 DEFAULT section 继承。
;; option 名大小写不敏感，存储时一律转为小写。
;;
;; 错误处理
;; ----
;; config-error
;; section 不存在或 option 不存在（含 DEFAULT 继承查找）时抛出。

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

;; 基本获取
(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\nport=5432\n")
  (check (config-get config "database" "host") => "localhost")
  (check (config-get config "database" "port") => "5432")
) ;let

;; DEFAULT section 继承
(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  ;; forge.example 没有 Compression，应从 DEFAULT 继承
  (check (config-get config "forge.example" "compression") => "yes")
  ;; topsecret.server.example 覆盖了 ForwardX11
  (check (config-get config "topsecret.server.example" "forwardx11") => "no")
) ;let

;; keys 不区分大小写
(let ((config (make-config-parser)))
  (config-read-string config "[Section]\nMyKey=value\n")
  (check (config-get config "Section" "mykey") => "value")
  (check (config-get config "Section" "MYKEY") => "value")
  (check (config-get config "Section" "MyKey") => "value")
) ;let

;; section 不存在时报错
(check-catch 'config-error
  (let ((config (make-config-parser)))
    (config-get config "missing" "key")
  ) ;let
) ;check-catch

;; option 不存在时报错
(check-catch 'config-error
  (let ((config (make-config-parser)))
    (config-read-string config "[database]\nhost=localhost\n")
    (config-get config "database" "missing")
  ) ;let
) ;check-catch

;; Python configparser 官方示例
(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  (check (config-get config "forge.example" "user") => "hg")
  (check (config-get config "DEFAULT" "compression") => "yes")
  (check (config-get config "forge.example" "compression") => "yes")
) ;let

(check-report)
