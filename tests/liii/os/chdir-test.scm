(import (liii check) (liii os))


(check-set-mode! 'report-failed)


;; chdir
;; 切换当前工作目录。
;;
;; 语法
;; ----
;; (chdir path)
;;
;; 参数
;; ----
;; path : string?
;; 目标目录路径。
;;
;; 返回值
;; -----
;; boolean?
;; 成功返回 #t。
;;
;; 错误
;; ----
;; file-not-found-error
;; 当目录不存在时抛出错误。


;; ; 基本功能测试
(let* ((orig-dir (getcwd))
       (test-dir (string-append (os-temp-dir) (string (os-sep)) "test_chdir_dir"))
      ) ;
  ;; 确保测试目录不存在
  (when (file-exists? test-dir)
    (rmdir test-dir)
  ) ;when

  ;; 创建测试目录
  (mkdir test-dir)
  (check-true (file-exists? test-dir))

  ;; 切换目录
  (check-true (chdir test-dir))

  ;; 验证当前目录已改变
  (let ((cwd (getcwd)))
    (check-true (or (string=? cwd test-dir) (string=? cwd (string-append "/private" test-dir)))
    ) ;check-true
  ) ;let

  ;; 切回原目录
  (chdir orig-dir)

  ;; 清理
  (rmdir test-dir)
  (check-false (file-exists? test-dir))
) ;let*


;; ; 错误测试
(check-catch 'file-not-found-error (chdir "/nonexistent/directory"))


(check-report)
