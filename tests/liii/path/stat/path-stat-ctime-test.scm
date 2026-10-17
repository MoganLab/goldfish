(import (liii check)
  (liii os)
  (liii path stat)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-ctime
;; 获取 <path-stat> 记录中的元数据改变或创建时间（Unix 时间戳，单位秒）。
;;
;; 语法
;; ----
;; (path-stat-ctime st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; real
;; 状态改变或创建时间戳（秒）。

(let ((st (make-path-stat 0 0 0 54321.123 0)))
  (check (path-stat-ctime st) => 54321.123)
) ;let

(let* ((tmp-dir (os-temp-dir)) (st (path-stat tmp-dir)))
  (check-true (> (path-stat-ctime st) 0))
) ;let*

(check-catch 'type-error (path-stat-ctime 123))
(check-catch 'type-error (path-stat-ctime "invalid"))

(check-report)
