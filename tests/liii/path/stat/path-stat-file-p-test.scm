(import (liii check)
  (liii os)
  (liii path stat)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-file?
;; 基于 <path-stat> 元数据判断是否为常规文件。
;;
;; 语法
;; ----
;; (path-stat-file? st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; boolean
;; 当模式位指示为普通文件时返回 #t，否则返回 #f。

(let ((custom-file (make-path-stat 10 0 0 0 32768)) ; S_IFREG
      (custom-dir (make-path-stat 10 0 0 0 16384)) ; S_IFDIR
     ) ;
  (check-true (path-stat-file? custom-file))
  (check-false (path-stat-file? custom-dir))
) ;let

(let ((test-file (string-append (os-temp-dir) "/path-stat-file-test.txt")))
  (when (file-exists? test-file)
    (delete-file test-file)
  ) ;when
  (with-output-to-file test-file (lambda () (display "hello")))
  (let ((st (path-stat test-file)))
    (check-true (path-stat-file? st))
  ) ;let
  (delete-file test-file)
) ;let

(let ((st-dir (path-stat (os-temp-dir))))
  (check-false (path-stat-file? st-dir))
) ;let

(check-catch 'type-error (path-stat-file? 123))
(check-catch 'type-error (path-stat-file? "not-a-record"))

(check-report)
