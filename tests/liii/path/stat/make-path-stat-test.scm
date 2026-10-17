(import (liii check)
  (liii path stat)
) ;import

(check-set-mode! 'report-failed)

;; make-path-stat
;; 构造一个 <path-stat> 元数据记录对象。
;;
;; 语法
;; ----
;; (make-path-stat size mtime atime ctime mode)
;;
;; 参数
;; ----
;; size : integer
;; 文件大小（字节）。
;; mtime : real
;; 最后修改时间戳（秒）。
;; atime : real
;; 最后访问时间戳（秒）。
;; ctime : real
;; 状态改变或创建时间戳（秒）。
;; mode : integer
;; 文件模式与权限位。
;;
;; 返回值
;; -----
;; <path-stat>
;; 构造出的元数据记录对象。

(let ((st (make-path-stat 42 100.5 200.5 300.5 33188)))
  (check-true (path-stat? st))
  (check (path-stat-size st) => 42)
  (check (path-stat-mtime st) => 100.5)
  (check (path-stat-atime st) => 200.5)
  (check (path-stat-ctime st) => 300.5)
  (check (path-stat-mode st) => 33188)
  (check-true (path-stat-file? st))
  (check-false (path-stat-dir? st))
  (check-false (path-stat-symlink? st))
) ;let

(check-report)
