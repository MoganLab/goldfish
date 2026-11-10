;; fix-cache-hit?
;; 判断指定文件是否已有 gf fix 缓存条目。
;;
;; 语法
;; ----
;; (fix-cache-hit? file-path)
;;
;; 参数
;; ----
;; file-path : string? 待检查缓存状态的文件路径
;;
;; 返回值
;; ------
;; boolean? 缓存条目存在时为 #t，否则为 #f

(import (scheme file)
  (liii check)
  (liii base)
  (liii goldfix cache)
  (liii tempfile)
) ;import

(check-set-mode! 'report-failed)

;; 测试：缓存条目不存在时为 #f，写入缓存条目后为 #t
(with-temp-file
  (lambda (file)
    (let ((cache (fix-cache-path file)))
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
      (check (fix-cache-hit? file) => #f)
      (fix-cache-touch file)
      (check (fix-cache-hit? file) => #t)
      (when (file-exists? cache)
        (delete-file cache)
      ) ;when
    ) ;let
  ) ;lambda
  "goldfix-cache-hit"
  ".scm"
) ;with-temp-file

(check-report)
