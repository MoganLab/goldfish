;; path-expanduser
;; 展开 ~ 为用户主目录(对齐 pathlib.Path.expanduser 的 ~ 部分;不支持 ~user)。
;;
;; 语法
;; ----
;; (path-expanduser path-value) -> path-value
;;
;; 参数
;; ----
;; path-value : path?
;; 待展开 ~ 前缀的输入路径。
;;
;; 返回值
;; ------
;; path-value
;; ~/x → home/x;无 ~ 前缀原样返回。

(import (liii check) (liii path) (liii os) (liii string))

(check-set-mode! 'report-failed)

(when (not (os-windows?))
  (check-true (path-absolute? (path-expanduser (path "~"))))
  (check (path->string (path-expanduser (path "foo/bar"))) => "foo/bar")
  ;; ~ 后接内容:展开结果以 home 为前缀
  (check-true
    (string-starts? (path->string (path-expanduser (path "~/x")))
      (path->string (path-home))
    ) ;string-starts?
  ) ;check-true
) ;when

(check-report)
