(import (liii check)
  (liii os)
  (liii path stat)
  (scheme file)
) ;import

(check-set-mode! 'report-failed)

;; path-stat-symlink?
;; 基于 <path-stat> 元数据判断是否为符号链接。
;;
;; 语法
;; ----
;; (path-stat-symlink? st)
;;
;; 参数
;; ----
;; st : <path-stat>
;; 元数据记录对象。
;;
;; 返回值
;; -----
;; boolean
;; 当模式位指示为符号链接时返回 #t，否则返回 #f。

(let ((custom-link (make-path-stat 10 0 0 0 40960)) ; S_IFLNK
      (custom-file (make-path-stat 10 0 0 0 32768)) ; S_IFREG
     ) ;
  (check-true (path-stat-symlink? custom-link))
  (check-false (path-stat-symlink? custom-file))
) ;let

(when (not (os-windows?))
  (let ((target-file (string-append (os-temp-dir) "/path-stat-target-symlink-test.txt"))
        (link-file (string-append (os-temp-dir) "/path-stat-link-symlink-test.txt"))
       ) ;
    (when (file-exists? link-file)
      (delete-file link-file)
    ) ;when
    (when (file-exists? target-file)
      (delete-file target-file)
    ) ;when
    (with-output-to-file target-file (lambda () (display "target content")))
    (os-call (string-append "ln -s " target-file " " link-file))
    (let ((st-link (path-stat link-file)))
      (check-true (path-stat-symlink? st-link))
      (check-false (path-stat-file? st-link))
      (check-false (path-stat-dir? st-link))
    ) ;let
    (delete-file link-file)
    (delete-file target-file)
  ) ;let
) ;when

(check-catch 'type-error (path-stat-symlink? 123))
(check-catch 'type-error (path-stat-symlink? "not-a-record"))

(check-report)
