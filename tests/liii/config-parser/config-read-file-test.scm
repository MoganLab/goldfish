(import (liii check)
  (liii config-parser)
  (liii raw-string)
  (scheme base)
) ;import

(check-set-mode! 'report-failed)

;; config-read-file
;; 从文件读取 INI 配置并填充到解析器。
;;
;; 语法
;; ----
;; (config-read-file config path)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; path : string
;; INI 配置文件的路径。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 文件内容解析规则与 `config-read-string` 一致。
;; 支持注释、多分隔符、DEFAULT section 继承等。

(let ((config (make-config-parser)))
  (config-read-file config "tests/liii/config-parser/test-data.ini")
  (check (config-sections config) => '("database"))
  (check (config-get config "database" "host") => "localhost")
  (check (config-get config "database" "port") => "5432")
) ;let

(check-report)
