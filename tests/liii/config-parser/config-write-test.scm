(import (liii check)
  (liii config-parser)
  (liii raw-string)
  (liii string)
  (scheme base)
) ;import

(check-set-mode! 'report-failed)

;; config-write
;; 将配置解析器的内容写出到端口。
;;
;; 语法
;; ----
;; (config-write config port)
;;
;; 参数
;; ----
;; config : <config-parser>
;; 配置解析器实例。
;;
;; port : output-port
;; 输出端口。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 输出格式为标准 INI 格式，使用 `key = value` 形式。
;; DEFAULT section（如果存在）会首先输出。

(let* ((config (make-config-parser))
       (_ (config-read-string config "[database]\nhost=localhost\nport=5432\n"))
       (result (call-with-output-string (lambda (port) (config-write config port))))
      ) ;
  (check (string-contains result "[database]") => #t)
  (check (string-contains result "host = localhost") => #t)
  (check (string-contains result "port = 5432") => #t)
) ;let*

(let* ((config (make-config-parser))
       (_ (config-read-string config "[DEFAULT]\nkey=val\n[section]\nother=data\n"))
       (result (call-with-output-string (lambda (port) (config-write config port))))
      ) ;
  (check (string-contains result "[DEFAULT]") => #t)
  (check (string-contains result "key = val") => #t)
  (check (string-contains result "other = data") => #t)
) ;let*

(check-report)
