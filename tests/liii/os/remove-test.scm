(import (liii check)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; remove
;; 删除文件。
;;
;; 语法
;; ----
;; (remove path)
;;
;; 参数
;; ----
;; path : string?
;; 要删除的文件路径。
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
;; file-not-found-error
;; 当文件不存在时抛出错误。
;;
;; value-error
;; 当尝试删除目录时抛出错误（提示使用 rmdir）。

;;; 基本功能测试
(when (or (os-macos?) (os-linux?))
  ;; 测试 remove
  (let ((test-file (string-append (os-temp-dir) "/test_remove.txt")))
    ;; 创建临时文件
    (with-output-to-file test-file
      (lambda () (display "test data"))
    ) ;with-output-to-file
    ;; 验证文件存在
    (check-true (file-exists? test-file))
    ;; 删除文件
    (check-true (remove test-file))
    ;; 验证文件已删除
    (check-false (file-exists? test-file))
  ) ;let
) ;when

;;; 错误测试
(check-catch 'type-error (remove 123))
(check-catch 'file-not-found-error (remove "/nonexistent/file"))

;;; 测试 remove 对目录的提示
(let ((test-dir (string-append (os-temp-dir) (string (os-sep)) "test_dir")))
  ;; 创建临时目录
  (when (not (file-exists? test-dir))
    (mkdir test-dir)
  ) ;when
  ;; 尝试删除目录，应提示使用 rmdir
  (check-catch 'value-error (remove test-dir))
  ;; 清理
  (rmdir test-dir)
  (when (file-exists? test-dir)
    (display* test-dir " failed to remove \n")
  ) ;when
) ;let

(check-report)
