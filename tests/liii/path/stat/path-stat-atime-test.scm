(import (liii check)
  (liii os)
  (liii path stat)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-atime
;; 获取 <path-stat> 记录中的最后访问时间（Unix 时间戳，单位秒）。
;;
;; 语法
;; ----
;; (path-stat-atime st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; real
;; 访问时间戳（秒）。

(let ((st (make-path-stat 0 0 98765.432 0 0)))
  (check (path-stat-atime st) => 98765.432)
) ;let

(let* ((tmp-dir (os-temp-dir)) (st (path-stat tmp-dir)))
  (check-true (> (path-stat-atime st) 0))
) ;let*

(check-catch 'type-error (path-stat-atime 123))
(check-catch 'type-error (path-stat-atime "invalid"))

(check-report)
