(import (liii check)
        (liii path)
        (liii os)
) ;import

(check-set-mode! 'report-failed)

;; path-rename
;; 重命名文件或目录。
;;
;; 语法
;; ----
;; (path-rename src dst)
;;
;; 参数
;; ----
;; src : path or string
;; 源路径。
;; dst : path or string
;; 目标路径。
;;
;; 返回值
;; ----
;; boolean
;; 成功返回 #t。
;;
;; 说明
;; ----
;; 如果源文件不存在，抛出 file-not-found-error。
;; 如果目标文件已存在，抛出 file-exists-error。
;;
;; 相关函数
;; ----
;; gf doc liii/os "rename"

;; 清理测试目录
(when (path-file? "tmp/rename-test-src.txt")
  (path-unlink "tmp/rename-test-src.txt")
) ;when
(when (path-file? "tmp/rename-test-dst.txt")
  (path-unlink "tmp/rename-test-dst.txt")
) ;when
(when (path-dir? "tmp/rename-test-dir1")
  (path-rmdir "tmp/rename-test-dir1")
) ;when
(when (path-dir? "tmp/rename-test-dir2")
  (path-rmdir "tmp/rename-test-dir2")
) ;when

;; 测试1: 重命名文件
(path-touch "tmp/rename-test-src.txt")
(check-true (path-rename "tmp/rename-test-src.txt" "tmp/rename-test-dst.txt"))
(check-false (path-exists? "tmp/rename-test-src.txt"))
(check-true (path-file? "tmp/rename-test-dst.txt"))

;; 测试2: 使用 path 对象重命名文件
(path-touch "tmp/rename-test-src.txt")
(check-true (path-rename (path "tmp/rename-test-src.txt") (path "tmp/rename-test-dst2.txt")))
(check-false (path-exists? "tmp/rename-test-src.txt"))
(check-true (path-file? "tmp/rename-test-dst2.txt"))
(path-unlink "tmp/rename-test-dst2.txt")

;; 测试3: 源文件不存在时抛出错误
(check-catch 'file-not-found-error
             (path-rename "tmp/nonexistent.txt" "tmp/dst.txt")
) ;check-catch

;; 测试4: 目标文件已存在时抛出错误
(path-touch "tmp/rename-test-src.txt")
(path-touch "tmp/rename-test-dst.txt")
(check-catch 'file-exists-error
             (path-rename "tmp/rename-test-src.txt" "tmp/rename-test-dst.txt")
) ;check-catch

;; 清理
(path-unlink "tmp/rename-test-src.txt")
(path-unlink "tmp/rename-test-dst.txt")

(check-report)
