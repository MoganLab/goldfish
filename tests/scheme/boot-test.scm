(import (liii list) (liii check) (liii os) (liii path))
(check-set-mode! 'report-failed)
(when (not (os-windows?))
  (check (file-exists? "/tmp") => #t)
  (check (file-exists? "/not_exists") => #f)
) ;when
(when (and (os-linux?) (not (string=? "root" (getlogin))))
  (check-catch 'permission-error (file-exists? "/root"))
) ;when
(when (os-windows?)
  (check (file-exists? "C:") => #t)
) ;when
(when (and (os-linux?) (not (string=? "root" (getlogin))))
  (check-catch 'permission-error (delete-file "/root"))
) ;when
(when (not (os-windows?))
  (with-output-to-file "/tmp/test_delete_file"
    (lambda () (display "Hello, World!"))
  ) ;with-output-to-file
  (check (file-exists? "/tmp/test_delete_file") => #t)
  (delete-file "/tmp/test_delete_file")
  (check (file-exists? "/tmp/test_delete_file") => #f)
) ;when

(define (sum start end)
  (if (= start end) start (+ (sum start (- end 1)) end))
) ;define
(check (sum 2 4) => 9)

;; 装饰导入（only/except/prefix/rename）不应绕过库加载缓存：
;; 同一个库无论以何种形式导入，磁盘文件只应被 load 一次。
;; import 是顶层模块表单（不能在 lambda 体内使用），所以先在顶层
;; 准备好探针库并把它所在目录加入 load-path，再依次做三个顶层
;; import；load 逐表单展开求值，load-path 的 set! 先于 import 生效。
(define probe-root (path-join (path-temp-dir)
                    (string-append "goldfish-import-cache-" (number->string (getpid)))
                  ) ;path-join
) ;define
(define probe-liii (path-join probe-root "liii"))
(define probe-file (path-join probe-liii "cacheprobe.scm"))
(define old-load-path *load-path*)
(mkdir (path->string probe-root))
(mkdir (path->string probe-liii))
(path-write-text probe-file
  (string-append "(define *cacheprobe-load-count*\n"
    "  (if (defined? '*cacheprobe-load-count*) (+ *cacheprobe-load-count* 1) 1))\n"
    "(define-library (liii cacheprobe)\n"
    "  (export probe-func)\n"
    "  (import (scheme base))\n"
    "  (begin (define (probe-func) 42)))\n"
  ) ;string-append
) ;path-write-text
(set! *load-path* (append *load-path* (list (path->string probe-root))))

(import (liii cacheprobe))
(import (only (liii cacheprobe) probe-func))
(import (rename (liii cacheprobe) (probe-func probe-func-renamed)))

(check *cacheprobe-load-count* => 1)
(check (probe-func) => 42)
(check (probe-func-renamed) => 42)

(set! *load-path* old-load-path)
(path-unlink probe-file #t)
(path-rmdir (path->string probe-liii))
(path-rmdir (path->string probe-root))

(check-report "\n\nCheck report of boot-test.scm => ")
