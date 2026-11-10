;; fix-cache-path
;; 根据文件内容的 SHA-256 摘要计算 gf fix 缓存条目路径：
;; 在缓存基目录下先建以摘要前两位命名的子目录，其余部分作为文件名。
;;
;; 语法
;; ----
;; (fix-cache-path file-path)
;;
;; 参数
;; ----
;; file-path : string? 待计算缓存路径的文件路径
;;
;; 返回值
;; ------
;; string? 缓存条目的路径字符串

(import (liii check)
  (liii base)
  (liii goldfix cache)
  (liii hashlib)
  (liii path)
  (liii tempfile)
) ;import

(check-set-mode! 'report-failed)

;; 测试：缓存路径由文件内容的 SHA-256 摘要决定
(with-temp-file
  (lambda (file)
    (let* ((hash (sha256-by-file file))
           (base (fix-cache-base-dir))
           (dir-part (substring hash 0 2))
           (name-part (substring hash 2))
           (expected (path->string (path-join base dir-part name-part)))
          ) ;
      (check (fix-cache-path file) => expected)
    ) ;let*
  ) ;lambda
  "goldfix-cache-path"
  ".scm"
) ;with-temp-file

(check-report)
