(import (liii check) (liii os) (liii path) (liii string))
(check-set-mode! 'report-failed)

;; g_library-file 返回当前正在加载的 Scheme 源文件的绝对路径，
;; 供库代码定位与自身源码同目录的资源文件（不受 cwd 影响）。

;; 顶层脚本：返回本测试文件自身的绝对路径
(let ((self (g_library-file)))
  (check (string? self) => #t)
  (check (path-absolute? (path self)) => #t)
  (check (string-ends? self "library-file-test.scm") => #t)
) ;let

;; eval 求值不会替换 loader port：仍返回当前正在加载的文件
(check (eval '(g_library-file) (rootlet)) => (g_library-file))

;; import 库文件：库体顶层求值时返回该库文件的绝对路径；
;; 库内过程在运行期被调用时，返回的是调用方正在加载的文件。
(let* ((probe-root
         (path-join (path-temp-dir)
           (string-append "goldfish-library-file-" (number->string (getpid)))
         ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (probe-file (path-join probe-liii "libfileprobe.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text probe-file
    (string-append "(define-library (liii libfileprobe)\n"
      "  (export libfileprobe-file libfileprobe-at-runtime)\n"
      "  (import (scheme base))\n" "  (begin\n"
      "    (define libfileprobe-file (g_library-file))\n"
      "    (define (libfileprobe-at-runtime) (g_library-file))))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind
    (lambda () (set! *load-path* (cons (path->string probe-root) *load-path*)))
    (lambda ()
      (import (liii libfileprobe))
      (check (string? libfileprobe-file) => #t)
      (check (path-absolute? (path libfileprobe-file)) => #t)
      (check (string-ends? libfileprobe-file "liii/libfileprobe.scm") => #t)
      ;; 运行期调用：返回调用方（本测试文件）的路径，而非库文件路径
      (check (string-ends? (libfileprobe-at-runtime) "library-file-test.scm") => #t)
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink probe-file #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

(check-report)
