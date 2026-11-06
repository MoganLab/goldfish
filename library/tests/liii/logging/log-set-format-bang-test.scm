;; log-set-format!
;; 设置日志格式串。
;;
;; 语法
;; ----
;; (log-set-format! format-string)
;;
;; 参数
;; ----
;; format-string : string
;; pyfmt 风格格式串，支持 asctime、levelname、severity、message 占位符。
;;
;; 返回值
;; ----
;; unspecified
;;
;; 说明
;; ----
;; 缺省格式为 "%(asctime)s [%(levelname)s] %(message)s"。

(import (liii check) (liii logging) (liii string))

(check-set-mode! 'report-failed)

(log-set-format! "[%(levelname)s] %(message)s")
(check (current-log-format) => "[%(levelname)s] %(message)s")

;; 格式对输出生效
(let ((p (open-output-string)))
  (default-log-handler '((SEVERITY . 6) (MESSAGE . "test")) p)
  (let ((output (get-output-string p)))
    (check (string-contains? output "[INFO] test") => #t)
  ) ;let
) ;let

;; 恢复默认
(log-set-format! "%(asctime)s [%(levelname)s] %(message)s")

(check-report)
