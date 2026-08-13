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
;; 同一个库无论以何种形式导入，磁盘文件只应被 load 一次
(let* ((probe-root (path-join (path-temp-dir)
                     (string-append "goldfish-import-cache-" (number->string (getpid)))
                   ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (probe-file (path-join probe-liii "cacheprobe.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text probe-file
    (string-append "(define *cacheprobe-load-count*\n"
      "  (if (defined? '*cacheprobe-load-count*) (+ *cacheprobe-load-count* 1) 1))\n"
      "(define-library (liii cacheprobe)\n" "  (export probe-func)\n"
      "  (import (scheme base))\n" "  (begin (define (probe-func) 42)))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind (lambda ()
                  (set! *load-path* (append *load-path* (list (path->string probe-root))))
                ) ;lambda
    (lambda ()
      (import (liii cacheprobe))
      (import (only (liii cacheprobe) probe-func))
      (import (rename (liii cacheprobe) (probe-func probe-func-renamed)))
      (check *cacheprobe-load-count* => 1)
      (check (probe-func) => 42)
      (check (probe-func-renamed) => 42)
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink probe-file #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

;; [0112_1] C 实现的 R7RS 库注册表基础设施
(let ((probe-env (inlet 'probe-x 1 'probe-y 2)))
  (check (g_library-defined? '(goldfish test-probe)) => #f)
  (check (g_library-ref '(goldfish test-probe)) => #f)
  (g_library-register! '(goldfish test-probe) probe-env)
  (check (g_library-defined? '(goldfish test-probe)) => #t)
  (check (eq? (g_library-ref '(goldfish test-probe)) probe-env) => #t)
  (check ((g_library-ref '(goldfish test-probe)) 'probe-x) => 1)
  ;; 重复注册：覆盖
  (let ((env2 (inlet 'probe-z 3)))
    (g_library-register! '(goldfish test-probe) env2)
    (check (eq? (g_library-ref '(goldfish test-probe)) env2) => #t)
    (check ((g_library-ref '(goldfish test-probe)) 'probe-z) => 3)
  ) ;let
  ;; 卸载
  (g_library-unregister! '(goldfish test-probe))
  (check (g_library-defined? '(goldfish test-probe)) => #f)
  (check (g_library-ref '(goldfish test-probe)) => #f)
) ;let

;; 库名必须是 proper list，元素为 symbol 或非负整数（R7RS）
(check-catch 'wrong-type-arg (g_library-defined? "not-a-list"))
(check-catch 'wrong-type-arg (g_library-defined? '(a . b)))
(check-catch 'wrong-type-arg (g_library-register! '(a 1.5) (inlet)))
(check-catch 'wrong-type-arg (g_library-register! '(a -1) (inlet)))
(check-catch 'wrong-type-arg (g_library-register! '(a) 42))

(check-report "\n\nCheck report of boot-test.scm => ")
