(import (liii check)
  (liii path)
) ;import

(check-set-mode! 'report-failed)

;; path-stat
;; 获取文件或目录的元数据记录对象（支持字符串或 path 记录对象）。
;;
;; 语法
;; ----
;; (path-stat path) → <path-stat>
;;
;; 参数
;; ----
;; path : string | path
;; 要获取元数据的文件或目录路径。
;;
;; 返回值
;; -----
;; <path-stat>
;; 返回 <path-stat> 记录对象。

(let ((test-path (path-join (path-temp-dir) "path-obj-stat-test.txt")))
  (when (path-exists? test-path)
    (path-unlink test-path)
  ) ;when
  (path-write-text test-path "testing path-stat on path object")
  ;; 传入 path 对象
  (let ((st1 (path-stat test-path))
        ;; 传入字符串
        (st2 (path-stat (path->string test-path)))
       ) ;
    (check-true (path-stat? st1))
    (check-true (path-stat? st2))
    (check (path-stat-size st1) => (path-stat-size st2))
    (check-true (path-stat-file? st1))
  ) ;let
  (path-unlink test-path)
) ;let

(check-catch 'file-not-found-error (path-stat (path "non-existent-1050-xyz")))
(check-catch 'type-error (path-stat 123))

(check-report)
