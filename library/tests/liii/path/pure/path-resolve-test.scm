;; path-resolve
;; 解析为规范化的绝对路径(对齐 pathlib.Path.resolve)。
;;
;; 语法
;; ----
;; (path-resolve path-value [strict]) -> path-value
;;
;; 参数
;; ----
;; path-value : path?
;; 待解析的输入路径。
;;
;; strict : boolean?
;; 可选，默认 #f。#t 时路径不存在抛 file-not-found-error。
;;
;; 返回值
;; ------
;; path-value
;; 解析符号链接并折叠 . / .. 段后的物理绝对路径。
;; 非 strict 模式下路径不存在时，解析已存在前缀并词法折叠剩余段。
;; Windows 无 realpath(3)：仅折叠段，不解析符号链接。

(import (liii check) (liii os) (liii path) (liii tempfile) (scheme file))

(check-set-mode! 'report-failed)

;; 词法折叠(不依赖文件系统存在性)
(when (not (os-windows?))
  (check (path->string (path-resolve (path "/a/b/.."))) => "/a")
  (check (path->string (path-resolve (path "/a/./b"))) => "/a/b")
  (check (path->string (path-resolve (path "/a/../.."))) => "/")
  (check-true (path-absolute? (path-resolve (path "foo"))))
) ;when

;; [1265] 符号链接解析：macOS $TMPDIR 为 /var -> /private/var 链接，
;; path-resolve 应统一返回物理形态
(when (not (os-windows?))
  (let ((resolved-tmp (path->string (path-resolve (os-temp-dir)))))
    ;; 解析后必然是绝对路径，且不再含 .. / . 段
    (check-true (path-absolute? (path resolved-tmp)))
    (check-false (member ".." (vector->list (path-parts (path resolved-tmp)))))
    ;; 与 os-temp-dir 指向同一目录：在其下建文件，物理路径下可见
    (let ((probe (path-join resolved-tmp "path-resolve-probe-1265.txt")))
      (with-output-to-file (path->string probe) (lambda () (display "x")))
      (check-true (file-exists? (path->string probe)))
      (delete-file (path->string probe))
    ) ;let
  ) ;let

  ;; 符号链接目录解析：link -> real，resolve(link) => real 的物理形态
  (let* ((base (make-temp-dir "path-resolve-link-"))
         (real-dir (path-join base "real"))
         (link-dir (path-join base "link"))
        ) ;
    (mkdir (path->string real-dir))
    (os-call (string-append "ln -s " (path->string real-dir) " " (path->string link-dir)))
    (check (path->string (path-resolve link-dir))
      => (path->string (path-resolve real-dir))
    ) ;check
    ;; strict 模式
    (check (path->string (path-resolve link-dir #t))
      => (path->string (path-resolve real-dir #t))
    ) ;check
    (delete-file (path->string link-dir))
    (rmdir (path->string real-dir))
    (rmdir base)
  ) ;let*

  ;; strict 模式：不存在路径抛 file-not-found-error
  (check-catch 'file-not-found-error
    (path-resolve "/nonexistent-root-1265/a/b" #t)
  ) ;check-catch

  ;; 非 strict：不存在路径解析已存在前缀 + 词法折叠
  (check (path->string (path-resolve "/nonexistent-root-1265/a/../b"))
    => "/nonexistent-root-1265/b"
  ) ;check
) ;when

(check-report)
