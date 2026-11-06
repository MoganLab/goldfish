;; os-arch
;; 获取当前系统的 CPU 架构字符串。
;;
;; 语法
;; ----
;; (os-arch)
;;
;; 参数
;; ----
;; 无参数。
;;
;; 返回值
;; -----
;; string?
;; 返回 CPU 架构，如 "x86_64"、"aarch64" 等。

(import (liii check) (liii os) (liii string))

(check-set-mode! 'report-failed)

;; 返回非空架构字符串
(let ((arch (os-arch)))
  (check-true (string? arch))
  (check-true (not (string-null? arch)))
) ;let

(check-report)
