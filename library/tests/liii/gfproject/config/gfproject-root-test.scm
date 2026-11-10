;; gfproject-root
;; 返回项目根目录路径。
;;
;; 语法
;; ----
;; (gfproject-root)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string 或 #f
;; 项目根目录路径；不在项目中时为 #f。
;;
;; 说明
;; ----
;; 项目根目录下存放 gfproject.json。

(import (liii check) (liii gfproject config))

(check-set-mode! 'report-failed)

(let ((root (gfproject-root)))
  (if root
    (begin
      (check-true (string? root))
      (check-true (> (string-length root) 0))
    ) ;begin
    (check-true #t)
  ) ;if
) ;let

(check-report)
