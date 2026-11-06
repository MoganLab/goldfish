;; isdir
;; 判断路径是否为已存在的目录。
;;
;; 语法
;; ----
;; (isdir path)
;;
;; 参数
;; ----
;; path : string?
;; 待检测的目录路径。
;;
;; 返回值
;; -----
;; boolean?
;; 路径存在且为目录返回 #t，不存在或不是目录返回 #f。
;;
;; 错误处理
;; ----
;; type-error
;; 当 path 不是字符串时抛出错误。

(import (liii check) (liii os))

(check-set-mode! 'report-failed)

(check-true (isdir "tests"))
(check-false (isdir "gf_check.json"))
(check-false (isdir "tests/no-such-directory"))
(check-catch 'type-error (isdir 123))

(check-report)
