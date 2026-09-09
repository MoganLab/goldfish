(import (liii check) (goldfish) (liii os))

;; per-level 实例化：同库不同 level 独立求值 cells。
;;
;; - registry 按 level 键：level 0 裸名，level >= 1 为 (level . name)；
;; - 同库 plain + expand 并存时体各跑一次（共享计数器验证两次）；
;; - add-import-view! 同库豁免，plain + expand 并存不报冲突；
;; - 两实例求值 cells 隔离（level 0 在 rootlet，level 1 在持有 inlet；
;;   冷缓存首载的两个 registry 项暂共享同一 exp-library 对象，
;;   但 baked gensym 在不同 inlet 求值即不同 cell，变异隔离成立；
;;   绑定对象彻底分离待运行时模块按 level 命名后补足）。

(define fixture-dir (os-temp-dir))
(define fixture-sub (string-append fixture-dir "/plvl"))
(catch #t (lambda () (mkdir fixture-sub)) (lambda args #f))

(call-with-output-file (string-append fixture-sub "/counter.scm")
  (lambda (p)
    (write '(define-library (plvl counter)
              (import (goldfish))
              (export counter-bump! counter-get counter-reset!)
              (begin
                (define nbox (vector 0))
                (define (counter-bump!) (vector-set! nbox 0 (+ (vector-ref nbox 0) 1)))
                (define (counter-get) (vector-ref nbox 0))
                (define (counter-reset!) (vector-set! nbox 0 0))))
           p)
    (newline p)))

(call-with-output-file (string-append fixture-sub "/dual.scm")
  (lambda (p)
    (write '(define-library (plvl dual)
              (import (goldfish) (plvl counter))
              (export get-v set-v! vbox)
              (begin
                (define vbox (vector 10))
                (define (get-v) (vector-ref vbox 0))
                (define (set-v! x) (vector-set! vbox 0 x))
                (define _load (begin (counter-bump!) 0))))
           p)
    (newline p)))

(if (not (member fixture-dir *load-path*))
  (set! *load-path* (cons fixture-dir *load-path*)))

;; 共享计数器经运行时模块访问（免导入，避免展开期未绑定）。
(load-library! '(plvl counter))
(define (plvl-counter-reset!) ((module-ref '(plvl counter) 'counter-reset!)))
(define (plvl-counter-get) ((module-ref '(plvl counter) 'counter-get)))
(plvl-counter-reset!)

;; 同库两 level 各实例化一次。
(load-library! '(plvl dual))
(load-library! '(plvl dual) 1)

;; ===== 1. 体跑两次 =====
(check (plvl-counter-get) => 2)

;; ===== 2. 两 registry 项独立（per-level 键）=====
(define rec0 (library-registry-ref '(plvl dual)))
(define rec1 (library-registry-ref '(plvl dual) 1))
(check-true (if (and rec0 rec1) #t #f))
;; 同一 level 经 library-record 复用，不再跑体。
(library-record '(plvl dual))
(library-record '(plvl dual) 1)
(check (plvl-counter-get) => 2)

;; ===== 3. 变异隔离：两 cells 独立 =====
(define vb0 (exp-library-ref (car rec0) 'vbox))
(define vb1 (exp-library-ref (car rec1) 'vbox))
(define g0 (toplevel-ref-gensym (binding-value vb0)))
(define g1 (toplevel-ref-gensym (binding-value vb1)))
(define inlet1 (instance-inlet-ref '(plvl dual) 1))
(check (vector? (eval g0 (rootlet))) => #t)
(check-true (if inlet1 #t #f))
;; 两边初值均为 10。
(check (vector-ref (eval g0 (rootlet)) 0) => 10)
(check (vector-ref (eval g1 inlet1) 0) => 10)
;; 改 level 0，不影响 level 1。
(vector-set! (eval g0 (rootlet)) 0 99)
(check (vector-ref (eval g0 (rootlet)) 0) => 99)
(check (vector-ref (eval g1 inlet1) 0) => 10)
;; 改 level 1，不影响 level 0。
(vector-set! (eval g1 inlet1) 0 77)
(check (vector-ref (eval g1 inlet1) 0) => 77)
(check (vector-ref (eval g0 (rootlet)) 0) => 99)

;; ===== 4. 同库豁免：plain + expand 并存不报冲突 =====
(define (write-program name . texts)
  (let ((src (string-append (os-temp-dir) "/gf-" name ".scm")))
    (call-with-output-file src (lambda (p) (display (apply string-append texts) p) (newline p)))
    src))
(define (clear-artifact! src)
  (let ((base (string-append (compile-cache-dir) "/" (cache-key-path src))))
    (for-each (lambda (suffix)
                (let ((f (string-append base suffix ".gfo")))
                  (when (file-exists? f) (delete-file f))))
              '("" "-o1" "-o2" "-o3"))))
(define src-mix
  (write-program "plvl-mix"
    "(import (goldfish))\n"
    "(import (plvl dual))\n"
    "(import (for (plvl dual) expand))\n"
    "(define f get-v)\n"))
(clear-artifact! src-mix)
(check-true (pair? (syntax->datum (compile-file-into src-mix (make-program-library)))))
(delete-file src-mix)

(check-report)
