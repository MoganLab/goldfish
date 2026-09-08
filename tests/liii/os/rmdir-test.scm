(import (liii check) (liii os))


(check-set-mode! 'report-failed)


;; rmdir
;; 删除空目录。
;;
;; 语法
;; ----
;; (rmdir path)
;;
;; 参数
;; ----
;; path : string?
;; 要删除的目录路径。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t。
;;
;; 错误
;; ----
;; type-error
;; 当 path 不是字符串时抛出错误。
;;
;; 说明
;; ----
;; 只能删除空目录，如果目录不为空会失败。


;; ; 基本功能测试
(let* ((temp-dir (os-temp-dir))
       (test-dir (string-append temp-dir (string (os-sep)) "test_rmdir_dir"))
      ) ;
  ;; 确保测试目录不存在
  (when (file-exists? test-dir)
    (rmdir test-dir)
  ) ;when

  ;; 创建测试目录
  (mkdir test-dir)
  (check-true (file-exists? test-dir))

  ;; 删除目录
  (check-true (rmdir test-dir))
  (check-false (file-exists? test-dir))
) ;let*


;; ; 错误测试
(check-catch 'type-error (rmdir #\a))
(check-catch 'type-error (rmdir 123))


(check-report)
