(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-get-boolean
;; 从配置解析器中获取指定 option 的布尔值。
;;
;; 语法
;; ----
;; (config-get-boolean config section option)
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
;; option 对应的布尔值。
;;
;; 说明
;; ----
;; 真值识别：yes, true, on, 1
;; 假值识别：no, false, off, 0
;; 大小写不敏感。支持 DEFAULT section 继承。
;;
;; 错误处理
;; ----
;; config-error
;; 值不是合法布尔字符串时抛出。

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
  (check (config-get-boolean config "topsecret.server.example" "forwardx11")
    =>
    #f
  ) ;check
  (check (config-get-boolean config "DEFAULT" "compression") => #t)
) ;let

;; getboolean 支持多种真/假值
(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      [section]
      a=yes
      b=true
      c=on
      d=1
      e=no
      f=false
      g=off
      h=0
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-get-boolean config "section" "a") => #t)
  (check (config-get-boolean config "section" "b") => #t)
  (check (config-get-boolean config "section" "c") => #t)
  (check (config-get-boolean config "section" "d") => #t)
  (check (config-get-boolean config "section" "e") => #f)
  (check (config-get-boolean config "section" "f") => #f)
  (check (config-get-boolean config "section" "g") => #f)
  (check (config-get-boolean config "section" "h") => #f)
) ;let

(check-report)
