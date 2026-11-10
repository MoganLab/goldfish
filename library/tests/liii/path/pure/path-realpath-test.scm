;; path-realpath
;; 解析符号链接并折叠 . / .. 段，返回物理绝对路径(对齐 os.path.realpath)。
;;
;; 语法
;; ----
;; (path-realpath path-value) -> path-value
;;
;; 参数
;; ----
;; path-value : path?
;; 待解析的输入路径，相对路径以 cwd 为种子。
;;
;; 返回值
;; ------
;; path-value
;; 物理绝对路径。等价于 (path-resolve p #f)。
;; Windows 无 realpath(3)：仅折叠段，不解析符号链接。

(import (liii check) (liii os) (liii path) (liii tempfile) (scheme file))

(check-set-mode! 'report-failed)

;; 与 path-resolve 等价
(check (path->string (path-realpath "/a/b/..")) => (path->string (path-resolve "/a/b/..")))

(when (not (os-windows?))
  ;; [1265] /var 为 /private/var 的符号链接(macOS)
  (check (path->string (path-realpath "/var"))
    => (path->string (path-resolve "/var"))
  ) ;check
  (check (path->string (path-realpath "/tmp"))
    => (path->string (path-realpath (path->string (path-realpath "/tmp"))))
  ) ;check

  ;; 符号链接文件：realpath(link) 与 realpath(target) 一致
  (let* ((base (make-temp-dir "path-realpath-"))
         (target (path-join base "target.txt"))
         (link (path-join base "link.txt"))
        ) ;
    (with-output-to-file (path->string target) (lambda () (display "t")))
    (os-call (string-append "ln -s " (path->string target) " " (path->string link)))
    (check (path->string (path-realpath link))
      => (path->string (path-realpath target))
    ) ;check
    ;; path-realpath 返回物理形态：与 target 的 resolve 结果一致
    (check (path->string (path-realpath link))
      => (path->string (path-resolve target))
    ) ;check
    (delete-file (path->string link))
    (delete-file (path->string target))
    (rmdir base)
  ) ;let*
) ;when

;; 类型错误
(check-catch 'type-error (path-realpath 123))

(check-report)
