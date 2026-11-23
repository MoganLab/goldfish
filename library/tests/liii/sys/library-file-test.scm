;; library-file
;; 返回当前正在加载的 Scheme 源文件的绝对路径。
;;
;; 语法
;; ----
;; (library-file)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; string 或 #f
;; 当前正在加载的源文件的绝对路径；无文件加载时返回 #f。
;;
;; 说明
;; ----
;; 库代码应在库体顶层调用并缓存结果，以定位与自身源码同目录的资源
;; 文件（不受 cwd 影响）；库内过程在运行期被调用时，返回的是调用方
;; 正在加载的文件。

(import (liii check)
        (liii os)
        (liii path)
        (liii string)
        (liii sys))

(check-set-mode! 'report-failed)

;; 顶层脚本：返回本测试文件自身的绝对路径
(let ((self (library-file)))
  (check-true (string? self))
  (check-true (path-absolute? (path self)))
  (check-true (string-ends? self "library-file-test.scm"))
) ;let

;; eval 求值不会替换 loader port：仍返回当前正在加载的文件
(let ((e (inlet 'library-file library-file)))
  (check (eval '(library-file) e) => (library-file))
) ;let

;; import 库文件：库体顶层求值时返回该库文件的绝对路径
(let* ((probe-root (path-join (path-temp-dir)
                     (string-append "liii-sys-library-file-" (number->string (getpid)))
                   ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (probe-file (path-join probe-liii "syslibfileprobe.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text probe-file
    (string-append "(define-library (liii syslibfileprobe)\n"
      "  (export probe-file probe-at-runtime)\n"
      "  (import (scheme base) (liii sys))\n"
      "  (begin\n"
      "    (define probe-file (library-file))\n"
      "    (define (probe-at-runtime) (library-file))))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind (lambda ()
                  (set! *load-path* (cons (path->string probe-root) *load-path*))
                ) ;lambda
    (lambda ()
      (import (liii syslibfileprobe))
      (check-true (string? probe-file))
      (check-true (path-absolute? (path probe-file)))
      (check-true (string-ends? probe-file "liii/syslibfileprobe.scm"))
      ;; 运行期调用：返回调用方（本测试文件）的路径，而非库文件路径
      (check-true (string-ends? (probe-at-runtime) "library-file-test.scm"))
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink probe-file #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

;; 参数个数校验（无参函数）
(check-catch 'wrong-number-of-args (library-file 1))

(check-report)