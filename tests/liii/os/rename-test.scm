(import (liii check)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; rename
;; 重命名文件或目录。
;;
;; 语法
;; ----
;; (rename src dst)
;;
;; 参数
;; ----
;; src : string?
;; 源文件或目录路径。
;;
;; dst : string?
;; 目标文件或目录路径。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t。
;;
;; 错误
;; ----
;; type-error
;; 当 src 或 dst 不是字符串时抛出错误。
;;
;; file-not-found-error
;; 当源文件不存在时抛出错误。
;;
;; file-exists-error
;; 当目标文件已存在时抛出错误。
;;
;; 相关函数
;; ----
;; gf doc liii/path "path-rename"

;;; 测试文件重命名
(let* ((temp-dir (os-temp-dir))
       (src-file (string-append temp-dir (string (os-sep)) "test_rename_src.txt"))
       (dst-file (string-append temp-dir (string (os-sep)) "test_rename_dst.txt")))
  ;; 创建源文件
  (with-output-to-file src-file
    (lambda () (display "test data for rename"))
  ) ;with-output-to-file
  ;; 验证源文件存在
  (check-true (file-exists? src-file))
  ;; 重命名文件
  (check-true (rename src-file dst-file))
  ;; 验证源文件不存在
  (check-false (file-exists? src-file))
  ;; 验证目标文件存在
  (check-true (file-exists? dst-file))
  ;; 清理
  (remove dst-file)
) ;let*

;;; 测试 rename 错误情况
(check-catch 'type-error (rename 123 "dst"))
(check-catch 'type-error (rename "src" 123))
(check-catch 'file-not-found-error (rename "/nonexistent/file.txt" "dst.txt"))

;;; 测试目标文件已存在
(let* ((temp-dir (os-temp-dir))
       (src-file (string-append temp-dir (string (os-sep)) "test_rename_src2.txt"))
       (dst-file (string-append temp-dir (string (os-sep)) "test_rename_dst2.txt")))
  ;; 创建源文件和目标文件
  (with-output-to-file src-file
    (lambda () (display "source content"))
  ) ;with-output-to-file
  (with-output-to-file dst-file
    (lambda () (display "destination content"))
  ) ;with-output-to-file
  ;; 目标文件已存在时应抛出 file-exists-error
  (check-catch 'file-exists-error (rename src-file dst-file))
  ;; 清理
  (remove src-file)
  (remove dst-file)
) ;let*

;;; 测试目录重命名
(let* ((temp-dir (os-temp-dir))
       (src-dir (string-append temp-dir (string (os-sep)) "test_rename_dir_src"))
       (dst-dir (string-append temp-dir (string (os-sep)) "test_rename_dir_dst")))
  ;; 创建源目录
  (when (file-exists? src-dir)
    (rmdir src-dir)
  ) ;when
  (when (file-exists? dst-dir)
    (rmdir dst-dir)
  ) ;when
  (mkdir src-dir)
  ;; 验证源目录存在
  (check-true (file-exists? src-dir))
  ;; 重命名目录
  (check-true (rename src-dir dst-dir))
  ;; 验证源目录不存在
  (check-false (file-exists? src-dir))
  ;; 验证目标目录存在
  (check-true (file-exists? dst-dir))
  ;; 清理
  (rmdir dst-dir)
) ;let*

(check-report)
