;; fix-cache-base-dir
;; 返回 gf fix 缓存的基目录路径，位于用户主目录的
;; .cache/goldfish/fix/<版本号> 之下。
;;
;; 语法
;; ----
;; (fix-cache-base-dir)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ------
;; string? 缓存基目录的路径字符串

(import (liii check) (liii base) (liii goldfix cache) (liii path) (liii string))

(check-set-mode! 'report-failed)

;; 测试：返回的是字符串
(check (string? (fix-cache-base-dir)) => #t)

;; 测试：路径以 .cache/goldfish/fix/<版本号> 结尾
(check
  (string-ends? (path-as-posix (path (fix-cache-base-dir)))
    (string-append ".cache/goldfish/fix/" (version))
  ) ;string-ends?
  =>
  #t
) ;check

(check-report)
