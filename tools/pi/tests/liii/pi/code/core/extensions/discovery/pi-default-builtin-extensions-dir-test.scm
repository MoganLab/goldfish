;; pi-default-builtin-extensions-dir
;; 获取内置扩展目录的默认位置。
;;
;; 语法
;; ----
;; (pi-default-builtin-extensions-dir)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string
;; 内置扩展目录路径。

(import (scheme base)
        (liii check)
        (liii pi code core extensions discovery)
        (liii pi code core extensions types)
) ;import

(check-set-mode! 'report-failed)

(let ((dir (pi-default-builtin-extensions-dir)))
  (check-true (string? dir))
  (check-true (> (string-length dir) 0))
) ;let*

(check-report)
