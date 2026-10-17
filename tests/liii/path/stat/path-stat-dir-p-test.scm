(import (liii check)
  (liii os)
  (liii path stat)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-dir?
;; 基于 <path-stat> 元数据判断是否为目录。
;;
;; 语法
;; ----
;; (path-stat-dir? st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; boolean
;; 当模式位指示为目录时返回 #t，否则返回 #f。

(let ((custom-dir (make-path-stat 10 0 0 0 16384)) ; S_IFDIR
      (custom-file (make-path-stat 10 0 0 0 32768)) ; S_IFREG
     ) ;
  (check-true (path-stat-dir? custom-dir))
  (check-false (path-stat-dir? custom-file))
) ;let

(let ((st-dir (path-stat (os-temp-dir))))
  (check-true (path-stat-dir? st-dir))
) ;let

(check-catch 'type-error (path-stat-dir? 123))
(check-catch 'type-error (path-stat-dir? "not-a-record"))

(check-report)
