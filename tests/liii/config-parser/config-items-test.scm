(import (liii check) (liii config-parser) (liii raw-string) (scheme base))

(check-set-mode! 'report-failed)

;; config-items
;; 获取指定 section 下所有 (key . value) 对（含 DEFAULT 继承）。
;;
;; 语法
;; ----
;; (config-items config section)
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
;; list of pair
;; (key . value) 对列表，section 自身的 option 会覆盖 DEFAULT 中同名的。
;;
;; 错误处理
;; ----
;; config-error
;; section 不存在时抛出。

(let ((config (make-config-parser)))
  (config-read-string config "[database]\nhost=localhost\nport=5432\n")
  (let ((items (config-items config "database")))
    (check (assoc "host" items) => '("host" . "localhost"))
    (check (assoc "port" items) => '("port" . "5432"))
  ) ;let
) ;let

(check-report)
