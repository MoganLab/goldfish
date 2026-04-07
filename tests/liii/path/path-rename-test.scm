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

(let* ((temp-dir (path-temp-dir))
       (src-file (path-join temp-dir "rename-test-src.txt"))
       (dst-file (path-join temp-dir "rename-test-dst.txt"))
       (dst2-file (path-join temp-dir "rename-test-dst2.txt"))
       (nonexistent-file (path-join temp-dir "nonexistent.txt")))

  ;; 清理可能存在的旧测试文件
  (when (path-file? src-file)
    (path-unlink src-file)
  ) ;when
  (when (path-file? dst-file)
    (path-unlink dst-file)
  ) ;when
  (when (path-file? dst2-file)
    (path-unlink dst2-file)
  ) ;when

  ;; 测试1: 重命名文件
  (path-touch src-file)
  (check-true (path-rename src-file dst-file))
  (check-false (path-exists? src-file))
  (check-true (path-file? dst-file))

  ;; 测试2: 使用 path 对象重命名文件
  (path-touch src-file)
  (check-true (path-rename (path src-file) (path dst2-file)))
  (check-false (path-exists? src-file))
  (check-true (path-file? dst2-file))
  (path-unlink dst2-file)

  ;; 测试3: 源文件不存在时抛出错误
  (check-catch 'file-not-found-error
               (path-rename nonexistent-file dst-file)
  ) ;check-catch

  ;; 测试4: 目标文件已存在时抛出错误
  (path-touch src-file)
  (path-touch dst-file)
  (check-catch 'file-exists-error
               (path-rename src-file dst-file)
  ) ;check-catch

  ;; 清理
  (path-unlink src-file)
  (path-unlink dst-file)
) ;let*

(check-report)
