(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-read-string
;; 从字符串读取 INI 配置并填充到解析器。
;;
;; 语法
;; ----
;; (config-read-string config str)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; str : string
;; 包含 INI 格式配置的字符串。
;;
;; 返回值
;; ----
;; <config-parser>
;; 返回传入的 config 实例。
;;
;; 说明
;; ----
;; 支持 `=` 和 `:` 作为分隔符，`;` 和 `#` 作为注释符。
;; 键名存储为小写（大小写不敏感）。前后空白被忽略。

(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      [database]
      host=localhost
      port=5432
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-sections config) => '("database"))
  (check (config-get config "database" "host") => "localhost")
  (check (config-get config "database" "port") => "5432")
) ;let

;; 前后空白被忽略
(let ((config (make-config-parser)))
  (config-read-string config "[section]\n  key  =  value  \n")
  (check (config-get config "section" "key") => "value")
) ;let

;; 注释和空行被忽略
(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      ; comment

      [section]
      ; another comment
      key=val
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-sections config) => '("section"))
  (check (config-get config "section" "key") => "val")
) ;let

;; 冒号分隔符
(let ((config (make-config-parser)))
  (config-read-string config "[section]\nkey: value\n")
  (check (config-get config "section" "key") => "value")
) ;let

;; 多个 = 只有第一个是分隔符
(let ((config (make-config-parser)))
  (config-read-string config "[section]\nurl=http://example.com?key=val\n")
  (check (config-get config "section" "url") => "http://example.com?key=val")
) ;let

(check-report)
