(import (liii check)
  (liii os)
  (liii path stat)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-mode
;; 获取 <path-stat> 记录中的文件类型与权限位（st_mode）。
;;
;; 语法
;; ----
;; (path-stat-mode st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; integer
;; 文件模式与权限位数值。

(let ((st (make-path-stat 0 0 0 0 33188)))
  (check (path-stat-mode st) => 33188)
) ;let

(let* ((tmp-dir (os-temp-dir)) (st (path-stat tmp-dir)))
  (check-true (> (path-stat-mode st) 0))
) ;let*

(check-catch 'type-error (path-stat-mode 123))
(check-catch 'type-error (path-stat-mode "invalid"))

(check-report)
