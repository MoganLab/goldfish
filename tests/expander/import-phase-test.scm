(import (liii check) (liii os) (goldfish))

;; R7RS (for import-set level ...) 的真 phase 可见性。
;;
;; 导入视图按 level 注册到导入方（run = 0，expand/syntax = 1，
;; (meta n) = n），resolve-identifier 在 phase >= level 时才查它：
;;   - for expand 导入的绑定在 transformer 体（phase+1）可用，
;;     在 phase-0 引用即 unbound-variable；
;;   - run 级导入与 plain 导入一样全程可见；
;;   - 同一库重复导入取更低 level（run+expand = 全程可见）；
;;   - for 可包裹 only 等 modifier，门控照常生效。
;;
;; 每个用例编译进一个全新的 (program ...) 库：导入视图挂在导入方
;; 身上，互不串染（会话库会被测试文件自身的导入污染，不能用）。

(define (write-program name . texts)
  (let ((src (string-append (os-temp-dir) "/gf-" name ".scm")))
    (call-with-output-file src (lambda (p) (display (apply string-append texts) p) (newline p)))
    src))

;; 编译前清产物：旧会话展开的缓存不能代表当前门控语义。
(define (clear-artifact! src)
  (let ((base (string-append (gfo-dir) "/" (gfo-key src))))
    (for-each (lambda (suffix)
                (let ((f (string-append base suffix ".gfo")))
                  (when (file-exists? f) (delete-file f))))
              '("" "-o1" "-o2" "-o3"))))

;; 全新 (program ...) 库：严格程序环境，unbound 在展开期报错；
;; 自带 core forms 与 module forms 种子，各用例互不串染。
(define (compile-fresh src)
  (clear-artifact! src)
  (compile-file-into src (make-program-library)))

(define (datum-contains? v target)
  (cond ((pair? v) (or (datum-contains? (car v) target)
                       (datum-contains? (cdr v) target)))
        ((vector? v) (let loop ((i 0))
                       (and (< i (vector-length v))
                            (or (datum-contains? (vector-ref v i) target)
                                (loop (+ i 1))))))
        (else (eqv? v target))))

;; ===== 1. for expand：transformer 体可用，phase-0 不可见 =====
(define src1
  (write-program "forp-1"
    "(import (goldfish))\n"
    "(import (for (scheme base) expand))\n"
    "(define-syntax m\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 10 (unsyntax (length (list 1 2 3)))))))))\n"
    "(define value (m))\n"))
;; transformer 体在 phase+1 展开：length 可调用（(length (list 1 2 3))
;; 求值为 3 折进产物），且 length 本身不再出现。
(let ((datum (syntax->datum (compile-fresh src1))))
  (check-true (datum-contains? datum 3))
  (check-true (not (datum-contains? datum 'length))))
(delete-file src1)

(define src2
  (write-program "forp-2"
    "(import (goldfish))\n"
    "(import (for (liii check) expand))\n"
    "(define f check-true)\n"))
(check-catch 'unbound-variable (compile-fresh src2))
(delete-file src2)

;; ===== 2. for run：与 plain 一致，全程可见 =====
(define src3
  (write-program "forp-3"
    "(import (goldfish))\n"
    "(import (for (liii check) run))\n"
    "(define f check-true)\n"))
(check-true (pair? (syntax->datum (compile-fresh src3))))
(delete-file src3)

;; ===== 3. (meta 2)：phase-0 不可见 =====
(define src4
  (write-program "forp-4"
    "(import (goldfish))\n"
    "(import (for (liii check) (meta 2)))\n"
    "(define f check-true)\n"))
(check-catch 'unbound-variable (compile-fresh src4))
(delete-file src4)

;; ===== 4. for 包裹 only：门控穿透 modifier =====
(define src5
  (write-program "forp-5"
    "(import (goldfish))\n"
    "(import (for (only (liii check) check-true) expand))\n"
    "(define f check-true)\n"))
(check-catch 'unbound-variable (compile-fresh src5))
(delete-file src5)

;; ===== 5. 多 level 取最小：run+expand 全程可见 =====
(define src6
  (write-program "forp-6"
    "(import (goldfish))\n"
    "(import (for (liii check) run expand))\n"
    "(define f check-true)\n"))
(check-true (pair? (syntax->datum (compile-fresh src6))))
(delete-file src6)

;; ===== 6. 重复导入放宽：plain 之后 for expand 不收紧 =====
(define src7
  (write-program "forp-7"
    "(import (goldfish))\n"
    "(import (liii check))\n"
    "(import (for (liii check) expand))\n"
    "(define f check-true)\n"))
(check-true (pair? (syntax->datum (compile-fresh src7))))
(delete-file src7)

;; ===== 7. 暖恢复路径：缓存记录的原始 spec 经 restore 保留 level =====
;; restore-library-cache 用记录里的原始 import spec 重建导入视图（捕获
;; 记录存的正是原始 spec）；手工构造一条同构记录，断言重建的库带着
;; level 门控（liii check 视图 level = 1，phase-0 不可见）。
(define rec (list '(forp v) '()
                  '(((goldfish) (for (liii check) expand)))
                  '() '() '()))
(define vlib (restore-library-cache rec))
(let find ((uses (exp-library-uses vlib)))
  (cond
    ((null? uses) #f)
    ((equal? '(liii check) (exp-library-name (caar uses)))
     (check (cdar uses) => 1)
     (check-false (exp-library-ref-at-phase vlib 'check-true 0))
     (check-true (if (exp-library-ref-at-phase vlib 'check-true 1) #t #f)))
    (else (find (cdr uses)))))

(check-report)
