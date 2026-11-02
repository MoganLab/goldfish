;; relativize-path
;; 将绝对路径转换为相对于当前项目根目录的相对路径并归一化分隔符。
;;
;; 语法
;; ----
;; (relativize-path path-str)
;;
;; 参数
;; ----
;; path-str : string   待转换的路径字符串
;;
;; 返回值
;; ------
;; string
;; 相对于项目根目录的路径，或者归一化后的原路径。

(import (scheme base)
  (liii check)
  (liii path)
  (liii gfproject config)
  (liii goldcheck path)
) ;import

(check-set-mode! 'report-failed)

;; 1. 相对路径归一化后返回
(check (relativize-path "foo/bar.scm") => "foo/bar.scm")
(check (relativize-path ".\\foo\\bar.scm") => "foo/bar.scm")

;; 2. 包含根目录前缀的绝对路径转换为相对路径
(let ((root (gfproject-root)))
  (if (and root (string? root) (not (string=? root "")))
    (let* ((full-path (path->string (path-join root "tools" "check" "main.scm")))
           (rel (relativize-path full-path))
          ) ;
      (check rel => "tools/check/main.scm")
    ) ;let*
  ) ;if
) ;let

(check-report)
