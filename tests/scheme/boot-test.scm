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

;; [0112_2] C 实现的 define-library
(define-library (goldfish test-lib-1)
  (export greet answer)
  (begin
    (define (greet) "hello")
    (define answer 42)
    (define hidden 99)
  ) ;begin
) ;define-library

(check (g_library-defined? '(goldfish test-lib-1)) => #t)
(let ((env (g_library-ref '(goldfish test-lib-1))))
  (check (procedure? (env 'greet)) => #t)
  (check ((env 'greet)) => "hello")
  (check (env 'answer) => 42)
  ;; 未导出的名字不在导出环境中
  (check (defined? 'hidden env) => #f)
) ;let

;; 与 Scheme 版 import 的互操作：C 定义的库可以被 import 引入
(import (goldfish test-lib-1))
(check (greet) => "hello")
(check answer => 42)
;; 加载缓存约定：库名对应的全局符号已定义，import 不会重复 load
(check (defined? (symbol (object->string '(goldfish test-lib-1)))) => #t)

;; 无 export 子句：库内所有绑定都被导出
(define-library (goldfish test-lib-2)
  (begin
    (define pub-1 1)
    (define pub-2 2)
  ) ;begin
) ;define-library
(let ((env (g_library-ref '(goldfish test-lib-2))))
  (check (env 'pub-1) => 1)
  (check (env 'pub-2) => 2)
) ;let

;; export 支持 (rename old new)
(define-library (goldfish test-lib-3)
  (export (rename internal external))
  (begin (define internal 7))
) ;define-library
(let ((env (g_library-ref '(goldfish test-lib-3))))
  (check (env 'external) => 7)
  (check (defined? 'internal env) => #f)
) ;let

;; 库体内 import：引入的绑定对 begin 中的代码可见
(define-library (goldfish test-lib-4)
  (export double-car)
  (import (scheme base))
  (begin (define (double-car x) (* 2 (car x))))
) ;define-library
(check (((g_library-ref '(goldfish test-lib-4)) 'double-car) '(3)) => 6)

;; 库体内 import 的名字可以被再导出
(define-library (goldfish test-lib-5)
  (export car)
  (import (only (scheme base) car))
) ;define-library
(check (((g_library-ref '(goldfish test-lib-5)) 'car) '(10 20)) => 10)

;; 导出未定义的名字：报错
(check-catch 'unbound-variable
  (define-library (goldfish test-lib-bad)
    (export missing-name)
  ) ;define-library
) ;check-catch

;; 非法库名：报错
(check-catch 'wrong-type-arg
  (define-library "not-a-list" (export x))
) ;check-catch

(check-report "\n\nCheck report of boot-test.scm => ")
