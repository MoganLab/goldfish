(import (liii check) (liii os))

;; isdir
;; 判断路径是否为已存在的目录。
;;
;; (isdir path)
;;
;; path : string?
;; 返回 boolean；路径不存在或不是目录时返回 #f。

(check-true (isdir "tests"))
(check-false (isdir "gf_check.json"))
(check-false (isdir "tests/no-such-directory"))
(check-catch 'type-error (isdir 123))

(check-report)
