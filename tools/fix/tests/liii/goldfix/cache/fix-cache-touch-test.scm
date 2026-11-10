;; fix-cache-touch
;; 为指定文件写入（或刷新）gf fix 缓存条目，缓存目录不存在时自动创建。
;;
;; 语法
;; ----
;; (fix-cache-touch file-path)
;;
;; 参数
;; ----
;; file-path : string? 待写入缓存条目的文件路径
;;
;; 返回值
;; ------
;; unspecified 写缓存操作没有返回值

(import (scheme file)
  (liii check)
  (liii base)
  (liii goldfix cache)
  (liii tempfile)
) ;import

(check-set-mode! 'report-failed)

;; 测试：写入缓存条目后缓存文件存在
(with-temp-file
  (lambda (file)
    (let ((cache (fix-cache-path file)))
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
      (fix-cache-touch file)
      (check (file-exists? cache) => #t)
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
    ) ;let
  ) ;lambda
  "goldfix-cache-touch"
  ".scm"
) ;with-temp-file

(check-report)
