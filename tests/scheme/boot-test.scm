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
    (define (greet)
      "hello"
    ) ;define
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
  (begin
    (define internal 7)
  ) ;begin
) ;define-library
(let ((env (g_library-ref '(goldfish test-lib-3))))
  (check (env 'external) => 7)
  (check (defined? 'internal env) => #f)
) ;let

;; 库体内 import：引入的绑定对 begin 中的代码可见
(define-library (goldfish test-lib-4)
  (export double-car)
  (import (scheme base))
  (begin
    (define (double-car x)
      (* 2 (car x))
    ) ;define
  ) ;begin
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
(check-catch 'wrong-type-arg (define-library "not-a-list" (export x)))

;; [0112_3] C 实现的 import：嵌套修饰符（旧 Scheme 实现不支持）
(let* ((probe-root (path-join (path-temp-dir)
                     (string-append "goldfish-import-nest-" (number->string (getpid)))
                   ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (probe-file (path-join probe-liii "nestprobe.scm"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  (path-write-text probe-file
    (string-append "(define *nestprobe-load-count*\n"
      "  (if (defined? '*nestprobe-load-count*) (+ *nestprobe-load-count* 1) 1))\n"
      "(define-library (liii nestprobe)\n"
      "  (export nest-a nest-b nest-c nest-d)\n"
      "  (begin (define nest-a 1) (define nest-b 2) (define nest-c 3) (define nest-d 4)))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind (lambda ()
                  (set! *load-path* (append *load-path* (list (path->string probe-root))))
                ) ;lambda
    (lambda ()
      ;; only 嵌套 except
      (import (only (except (liii nestprobe) nest-d) nest-a nest-b))
      (check nest-a => 1)
      (check nest-b => 2)
      (check (defined? 'nest-c) => #f)
      (check (defined? 'nest-d) => #f)
      ;; 嵌套修饰符不绕过加载缓存：文件只 load 一次
      (check *nestprobe-load-count* => 1)
      ;; prefix 嵌套 only
      (import (prefix (only (liii nestprobe) nest-a) pre-))
      (check pre-nest-a => 1)
      ;; rename 嵌套 except
      (import (rename (except (liii nestprobe) nest-a nest-b nest-d) (nest-c renamed-c))
      ) ;import
      (check renamed-c => 3)
      (check *nestprobe-load-count* => 1)
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink probe-file #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

;; 非法 import set：报错
(check-catch 'wrong-type-arg (import "not-a-library-name"))
(check-catch 'wrong-type-arg (import 42))
;; 修饰符结构不完整：报错
(check-catch 'syntax-error (import (only)))

;; [0112_4] define-library 支持 cond-expand 声明
;; 特性条件：r7rs 特性已注册
(define-library (goldfish test-lib-ce1)
  (export ce1-value)
  (cond-expand (r7rs (begin (define ce1-value 'from-r7rs)))
               (else (begin (define ce1-value 'from-else)))
  ) ;cond-expand
) ;define-library
(check ((g_library-ref '(goldfish test-lib-ce1)) 'ce1-value) => 'from-r7rs)

;; else 分支
(define-library (goldfish test-lib-ce2)
  (export ce2-value)
  (cond-expand (no-such-feature-xyz (begin (define ce2-value 1)))
               (else (begin (define ce2-value 2)))
  ) ;cond-expand
) ;define-library
(check ((g_library-ref '(goldfish test-lib-ce2)) 'ce2-value) => 2)

;; and/or/not 组合
(define-library (goldfish test-lib-ce3)
  (export ce3-value)
  (cond-expand ((and r7rs (not no-such-feature-xyz)) (begin (define ce3-value 'and-ok)))
               (else (begin (define ce3-value 'bad)))
  ) ;cond-expand
) ;define-library
(check ((g_library-ref '(goldfish test-lib-ce3)) 'ce3-value) => 'and-ok)

(define-library (goldfish test-lib-ce3b)
  (export ce3b-value)
  (cond-expand ((or no-such-feature-xyz (and r7rs no-such-feature-abc))
                (begin
                  (define ce3b-value 'bad)
                ) ;begin
               ) ;
               (else (begin (define ce3b-value 'or-ok)))
  ) ;cond-expand
) ;define-library
(check ((g_library-ref '(goldfish test-lib-ce3b)) 'ce3b-value) => 'or-ok)

;; (library ...) 条件：可加载的库
(define-library (goldfish test-lib-ce4)
  (export ce4-value)
  (cond-expand ((library (liii base)) (begin (define ce4-value 'has-base)))
               (else (begin (define ce4-value 'no-base)))
  ) ;cond-expand
) ;define-library
(check ((g_library-ref '(goldfish test-lib-ce4)) 'ce4-value) => 'has-base)

;; (library ...) 条件：不存在的库
(define-library (goldfish test-lib-ce5)
  (export ce5-value)
  (cond-expand ((library (no-such lib-xyz)) (begin (define ce5-value 'bad)))
               (else (begin (define ce5-value 'good)))
  ) ;cond-expand
) ;define-library
(check ((g_library-ref '(goldfish test-lib-ce5)) 'ce5-value) => 'good)

;; cond-expand 中的 export 子句
(define-library (goldfish test-lib-ce6)
  (cond-expand (r7rs (export ce6-func))
               (else)
  ) ;cond-expand
  (begin
    (define (ce6-func)
      6
    ) ;define
  ) ;begin
) ;define-library
(check (((g_library-ref '(goldfish test-lib-ce6)) 'ce6-func)) => 6)

;; cond-expand 中的 import 子句
(define-library (goldfish test-lib-ce7)
  (export ce7-car)
  (cond-expand (r7rs (import (only (scheme base) car)))
               (else)
  ) ;cond-expand
  (begin
    (define (ce7-car x)
      (car x)
    ) ;define
  ) ;begin
) ;define-library
(check (((g_library-ref '(goldfish test-lib-ce7)) 'ce7-car) '(9 8)) => 9)

;; cond-expand 嵌套
(define-library (goldfish test-lib-ce8)
  (export ce8-value)
  (cond-expand (r7rs (cond-expand (no-such-feature-xyz (begin (define ce8-value 'bad)))
                                  (else (begin (define ce8-value 'nested-ok)))
                     ) ;cond-expand
               ) ;r7rs
               (else (begin (define ce8-value 'bad2)))
  ) ;cond-expand
) ;define-library
(check ((g_library-ref '(goldfish test-lib-ce8)) 'ce8-value) => 'nested-ok)

;; 表达式级 cond-expand（C 版，同样支持 (library ...) 条件）
(check (cond-expand (r7rs 'r7rs-ok) (else 'bad)) => 'r7rs-ok)
(check (cond-expand (no-such-feature-xyz 'bad) (else 'else-ok)) => 'else-ok)
(check (cond-expand ((library (liii base)) 'lib-ok) (else 'bad)) => 'lib-ok)
(check (cond-expand ((and r7rs (not no-such-feature-xyz)) 'and-ok)
                    (else 'bad)
       ) ;cond-expand
  =>
  'and-ok
) ;check
(check (cond-expand ((or no-such-feature-xyz r7rs) 'or-ok)
                    (else 'bad)
       ) ;cond-expand
  =>
  'or-ok
) ;check

;; [0112_5] define-library 支持 include / include-ci / include-library-declarations
(let* ((probe-root (path-join (path-temp-dir)
                     (string-append "goldfish-include-" (number->string (getpid)))
                   ) ;path-join
       ) ;probe-root
       (probe-liii (path-join probe-root "liii"))
       (old-load-path *load-path*)
      ) ;
  (mkdir (path->string probe-root))
  (mkdir (path->string probe-liii))
  ;; 实现文件与声明文件都放在库文件旁边（include 按库文件所在目录解析）
  (path-write-text (path-join probe-liii "incimpl.scm")
    "(define (inc-func) 100)\n(define (inc-ci-func) 7)\n(define inc-hidden 1)\n"
  ) ;path-write-text
  (path-write-text (path-join probe-liii "incdecls.scm")
    "(export inc-func inc-ci-func)\n"
  ) ;path-write-text
  (path-write-text (path-join probe-liii "inclib.scm")
    (string-append "(define-library (liii inclib)\n"
      "  (include-library-declarations \"incdecls.scm\")\n"
      "  (import (scheme base))\n" "  (include \"incimpl.scm\")\n"
      "  (cond-expand (r7rs (include-ci \"incimpl.scm\")) (else)))\n"
    ) ;string-append
  ) ;path-write-text
  (dynamic-wind (lambda ()
                  (set! *load-path* (append *load-path* (list (path->string probe-root))))
                ) ;lambda
    (lambda ()
      (import (liii inclib))
      (check (inc-func) => 100)
      (check (inc-ci-func) => 7)
      ;; 未被 include-library-declarations 导出的名字不可见
      (check (defined? 'inc-hidden) => #f)
    ) ;lambda
    (lambda ()
      (set! *load-path* old-load-path)
      (path-unlink (path-join probe-liii "incimpl.scm") #t)
      (path-unlink (path-join probe-liii "incdecls.scm") #t)
      (path-unlink (path-join probe-liii "inclib.scm") #t)
      (path-rmdir (path->string probe-liii))
      (path-rmdir (path->string probe-root))
    ) ;lambda
  ) ;dynamic-wind
) ;let*

;; include 找不到文件：报错
(check-catch 'io-error
  (define-library (goldfish test-lib-incbad)
    (include "no-such-file-xyz.scm")
  ) ;define-library
) ;check-catch

(check-report "\n\nCheck report of boot-test.scm => ")
