;; goldtool-cache-path
;; 按目标文件内容的 SHA-256 计算共享缓存条目路径。
;; 路径形如 <缓存根目录>/<额外路径段...>/<哈希前两位>/<哈希剩余位>，
;; 相同内容、不同工具的缓存条目互不冲突。
;;
;; 语法
;; ----
;; (goldtool-cache-path tool-name file-path . extra-segments)
;;
;; 参数
;; ----
;; tool-name : string?
;; 工具名称。
;; file-path : string?
;; 目标文件的路径。
;; extra-segments : string? ...
;; 可选的额外路径段，用于隔离同一文件在不同用途下的缓存。
;;
;; 返回值
;; ------
;; string?
;; 缓存条目文件的绝对路径。

(import (scheme base)
  (liii check)
  (liii hashlib)
  (liii path)
  (liii string)
  (liii tempfile)
  (liii goldtool cache)
) ;import

(check-set-mode! 'report-failed)

;; 拼接缓存条目路径：<缓存根目录>/<额外路径段...>/<哈希前两位>/<哈希剩余位>
(define (expected-cache-path tool-name file . extra-segments)
  (let* ((hash (sha256-by-file file))
         (prefix (substring hash 0 2))
         (rest (substring hash 2))
         (base (goldtool-cache-base-dir tool-name))
        ) ;
    (path->string (apply path-join (append (list base) extra-segments (list prefix rest))))
  ) ;let*
) ;define

(with-temp-file
  (lambda (file)
    (path-write-text (path file) "goldfish goldtool cache\n")
    ;; 路径由内容哈希的两段目录构成
    (check (equal? (goldtool-cache-path "goldtool-test" file)
                   (expected-cache-path "goldtool-test" file)
          ) ;equal?
      => #t
    ) ;check
    ;; 额外路径段插入在哈希目录之前
    (check (equal? (goldtool-cache-path "goldtool-test" file "extra")
                   (expected-cache-path "goldtool-test" file "extra")
          ) ;equal?
      => #t
    ) ;check
    ;; 不同工具名对应不同缓存根目录
    (check (equal? (goldtool-cache-path "tool-a" file) (goldtool-cache-path "tool-b" file)) => #f)
  ) ;lambda
  "goldtool-cache-path"
  ".scm"
) ;with-temp-file

(check-report)
