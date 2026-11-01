;; check-config-exists?
;; 检查指定目录或工程根目录下是否存在 gf_check.json 配置文件。
;;
;; 语法
;; ----
;; (check-config-exists? [root])
;;
;; 参数
;; ----
;; root : string?（可选）
;; 待检查的目录路径。缺省时使用工程根目录或当前目录。
;;
;; 返回值
;; ----
;; boolean?
;; 若目标目录下存在 gf_check.json 返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii goldcheck config)
) ;import

(check-set-mode! 'report-failed)

;; 1. 当前工程根目录存在 gf_check.json
(check-true (check-config-exists?))

;; 2. 不存在的目录返回 #f
(check-false (check-config-exists? "/nonexistent/directory/xyz"))

;; 3. 临时目录无配置文件时返回 #f
(let ((tmp-dir (path->string (path-temp-dir))))
  (check-false (check-config-exists? (string-append tmp-dir "/nonexistent-sub-dir")))
) ;let

(check-report)
