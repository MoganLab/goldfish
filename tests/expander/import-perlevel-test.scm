(import (liii check) (goldfish) (liii os))

;; per-level 实例化：同库不同 level 独立实例。
;;
;; - registry 按 level 键：level 0 裸名，level >= 1 为 (level . name)；
;; - 同库 plain + expand 并存时体各跑一次（共享计数器验证）；
;; - 两实例 exp-library 对象与绑定对象分离（同 gensym 符号，
;;   不同 binding 对象，各自 inlet 求值即不同 cell）；
;; - add-import-view! 同库豁免，plain + expand 并存不报冲突；
;; - level-1 首载不污染 bare 注册（门控不被欺骗）。

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

(call-with-output-file (string-append fixture-sub "/solo.scm")
  (lambda (p)
    (write '(define-library (plvl solo)
              (import (goldfish) (plvl counter))
              (export sv)
              (begin
                (define sv (vector 1))
                (define _load (begin (counter-bump!) 0))))
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

(call-with-output-file (string-append fixture-sub "/formulti.scm")
  (lambda (p)
    (write '(define-library (plvl formulti)
              (import (goldfish) (for (plvl dual) expand (meta 2)))
              (export fok)
              (begin
                (define fok 1)))
           p)
    (newline p)))

(if (not (member fixture-dir *load-path*))
  (set! *load-path* (cons fixture-dir *load-path*)))

;; 共享计数器经运行时模块访问（免导入，避免展开期未绑定）。
(load-library! '(plvl counter))
(define (plvl-counter-reset!) ((module-ref '(plvl counter) 'counter-reset!)))
(define (plvl-counter-get) ((module-ref '(plvl counter) 'counter-get)))
(plvl-counter-reset!)

;; ===== 0. level-1 首载不污染 bare =====
(load-library! '(plvl solo) 1)
(check (plvl-counter-get) => 1)
(check (library-registry-ref '(plvl solo)) => #f)
(check (if (runtime-registered? '(plvl solo)) #t #f) => #f)
(check-true (if (library-registry-ref '(plvl solo) 1) #t #f))
(check-true (if (runtime-registered? '(plvl solo) 1) #t #f))
(load-library! '(plvl solo))
(check (plvl-counter-get) => 2)
(check-true (if (and (library-registry-ref '(plvl solo))
                     (library-registry-ref '(plvl solo) 1)) #t #f))
(plvl-counter-reset!)

;; 同库两 level 各实例化一次。
(load-library! '(plvl dual))
(load-library! '(plvl dual) 1)

;; ===== 1. 体跑两次 =====
(check (plvl-counter-get) => 2)

;; ===== 2. 两 registry 项独立（对象与绑定分离）=====
(define rec0 (library-registry-ref '(plvl dual)))
(define rec1 (library-registry-ref '(plvl dual) 1))
(check-true (if (and rec0 rec1) #t #f))
(check-true (if (not (eq? (car rec0) (car rec1))) #t #f))
(define b0 (exp-library-ref (car rec0) 'get-v))
(define b1 (exp-library-ref (car rec1) 'get-v))
(check-true (if (and b0 b1) #t #f))
(check-true (if (not (eq? b0 b1)) #t #f))
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

;; ===== 3b. level-0 运行时模块不被 level-1 加载覆盖 =====
;; 此时 level-0 vbox 为 99，level-1 为 77；module-ref 应命中 level-0。
(check ((module-ref '(plvl dual) 'get-v)) => 99)

;; ===== 3c. 显式第三层：(meta 2) 独立实例 =====
(load-library! '(plvl dual) 2)
(define rec2 (library-registry-ref '(plvl dual) 2))
(check-true (if rec2 #t #f))
(check-true (if (and (not (eq? (car rec2) (car rec0)))
                     (not (eq? (car rec2) (car rec1)))) #t #f))
(check-true (if (runtime-registered? '(plvl dual) 2) #t #f))
;; level-0 注册不受影响。
(check-true (if (and (library-registry-ref '(plvl dual))
                     (runtime-registered? '(plvl dual))) #t #f))
;; 第三实例 cells 独立，初值 10；改动不影响前两层。
(define inlet2 (instance-inlet-ref '(plvl dual) 2))
(check-true (if inlet2 #t #f))
(define vb2 (exp-library-ref (car rec2) 'vbox))
(define g2 (toplevel-ref-gensym (binding-value vb2)))
(check (vector-ref (eval g2 inlet2) 0) => 10)
(vector-set! (eval g2 inlet2) 0 55)
(check (vector-ref (eval g2 inlet2) 0) => 55)
(check (vector-ref (eval g0 (rootlet)) 0) => 99)
(check (vector-ref (eval g1 inlet1) 0) => 77)
;; level-0 运行时模块仍完好。
(check ((module-ref '(plvl dual) 'get-v)) => 99)

;; ===== 3d. 三层解析：phase 取最高 level ≤ 相位 =====
(define b2 (exp-library-ref (car rec2) 'get-v))
(define probe (make-exp-library '(plvl probe)))
(add-import-view! probe (import-view '(plvl dual) '((get-v . get-v)) #t 0) 0)
(add-import-view! probe (import-view '(plvl dual) '((get-v . get-v)) #t 1) 1)
(add-import-view! probe (import-view '(plvl dual) '((get-v . get-v)) #t 2) 2)
(check (if (eq? (exp-library-ref-at-phase probe 'get-v 0) b0) #t #f) => #t)
(check (if (eq? (exp-library-ref-at-phase probe 'get-v 1) b1) #t #f) => #t)
(check (if (eq? (exp-library-ref-at-phase probe 'get-v 2) b2) #t #f) => #t)

;; ===== 3e. 精确相位：{1, 2} 视图在 0 与 3+ 不可见 =====
;; run 持久之外的 level 只在精确相位可见（阈值模型会在 3+ 误命中）。
(define probe2 (make-exp-library '(plvl probe2)))
(add-import-view! probe2 (import-view '(plvl dual) '((get-v . get-v)) #t 1) 1)
(add-import-view! probe2 (import-view '(plvl dual) '((get-v . get-v)) #t 2) 2)
(check (exp-library-ref-at-phase probe2 'get-v 0) => #f)
(check (if (eq? (exp-library-ref-at-phase probe2 'get-v 1) b1) #t #f) => #t)
(check (if (eq? (exp-library-ref-at-phase probe2 'get-v 2) b2) #t #f) => #t)
(check (exp-library-ref-at-phase probe2 'get-v 3) => #f)

;; ===== 3f. 多 level `for' 取并：逐层各注册一视图 =====
;; (for dual expand (meta 2)) 在 formulti 身上留下 level 1 与 2 两个视图
;;（旧 min 语义只留 level 1）。
(load-library! '(plvl formulti))
(define fm-uses (exp-library-uses (car (library-registry-ref '(plvl formulti)))))
(define (fm-levels name)
  (let loop ((us fm-uses) (acc '()))
    (if (null? us)
      (reverse acc)
      (loop (cdr us)
            (if (equal? (exp-library-name (caar us)) name)
              (cons (cdar us) acc)
              acc)))))
(let ((ls (fm-levels '(plvl dual))))
  (check (length ls) => 2)
  (check-true (if (and (memv 1 ls) (memv 2 ls)) #t #f)))

;; ===== 4. 同库豁免：plain + expand 并存不报冲突 =====
(define (write-program name . texts)
  (let ((src (string-append (os-temp-dir) "/gf-" name ".scm")))
    (call-with-output-file src (lambda (p) (display (apply string-append texts) p) (newline p)))
    src))
(define (clear-artifact! src)
  (let ((base (string-append (gfo-dir) "/" (gfo-key src))))
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
