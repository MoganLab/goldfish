(import (liii check) (liii os) (goldfish))

;; eval-when (expand) / begin-for-syntax：展开期区域语义。
;;
;; 区域内的 define 只存在于 phase+1（专用 region 库 + phase 门控查找）：
;; 同区域与后续 form 的 transformer 体可以直接调用它们（展开期求值，
;; 编译期折叠为常量，产物自包含）；phase-0 引用在展开期即报
;; unbound-variable（Racket 语义），永不进缓存、无泄漏可言。

(define (write-program name . texts)
  (let ((src (string-append (os-temp-dir) "/gf-" name ".scm")))
    (call-with-output-file src (lambda (p) (display (apply string-append texts) p) (newline p)))
    src))

;; 泛化树包含检查（eqv? 匹配，数字等非 symbol 叶子也可用）。
(define (datum-contains? v target)
  (cond ((pair? v) (or (datum-contains? (car v) target)
                       (datum-contains? (cdr v) target)))
        ((vector? v) (let loop ((i 0))
                       (and (< i (vector-length v))
                            (or (datum-contains? (vector-ref v i) target)
                                (loop (+ i 1))))))
        (else (eqv? v target))))

;; 编译前清掉同名临时文件的旧产物：stamp 是秒级 mtime + size，同秒内
;; 重写同大小文件会陈旧命中（产品侧隐患，暂以测试防御规避）。
(define (clear-artifact! src)
  (let ((base (string-append (compile-cache-dir) "/" (cache-key-path src))))
    (for-each (lambda (suffix)
                (let ((f (string-append base suffix ".gfo")))
                  (when (file-exists? f) (delete-file f))))
              '("" "-o1" "-o2" "-o3"))))

;; ===== 1. 区域 define + transformer 调用：编译期折叠，无泄漏 =====
(define src1
  (write-program "ewx-1"
    "(import (goldfish))\n"
    "(eval-when (expand) (define (helper x) (+ x 1)))\n"
    "(define-syntax m\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 2 (unsyntax (helper 20))))))))\n"
    "(define value (m))\n"))
(let* ((opt (begin (clear-artifact! src1)
                   (compile-file-cached src1)))
       (datum (syntax->datum opt)))
  (check-true (pair? datum))
  ;; transformer 在展开期运行：helper 调用折叠为常量 42
  (check-true (datum-contains? datum 42)))
(delete-file src1)

;; ===== 2. begin-for-syntax：同一机制的 Racket 风格表面 =====
(define src2
  (write-program "ewx-2"
    "(import (goldfish))\n"
    "(begin-for-syntax (define (helper x) (* x 3)))\n"
    "(define-syntax m\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 2 (unsyntax (helper 20))))))))\n"
    "(define value (m))\n"))
(let* ((opt (begin (clear-artifact! src2)
                   (compile-file-cached src2)))
       (datum (syntax->datum opt)))
  ;; 同进程先后两个 compile 各自的区域：后一区域定义同名 helper，
  ;; 本文件折叠为 120（上一区域的 42 不应出现在本产物里）。
  (check-true (datum-contains? datum 120)))
(delete-file src2)

;; ===== 3. 合并区域：define 与 define-syntax 同处一个 eval-when =====
(define src3
  (write-program "ewx-3"
    "(import (goldfish))\n"
    "(eval-when (expand)\n"
    "  (define (helper x) (+ x 1))\n"
    "  (define-syntax m\n"
    "    (lambda (stx)\n"
    "      (syntax-case stx ()\n"
    "        ((_) (quasisyntax (* 2 (unsyntax (helper 20)))))))))\n"
    "(define value (m))\n"))
(let* ((opt (begin (clear-artifact! src3)
                   (compile-file-cached src3)))
       (datum (syntax->datum opt)))
  (check-true (datum-contains? datum 42)))
(delete-file src3)

;; ===== 4. 区域 define 不可见于运行期：编译期静态错误 =====
;; Racket 语义：region 绑定只存在于 phase+1。phase-0 引用在展开期
;; 即报 unbound-variable，不回退、不缓存、不泄漏。
(define src4
  (write-program "ewx-4"
    "(import (goldfish))\n"
    "(eval-when (expand) (define hidden 5))\n"
    "(define value hidden)\n"))
(clear-artifact! src4)
(check-catch 'unbound-variable (compile-file-cached src4))
(delete-file src4)

;; ===== 5. 展开期 set! 运行期变量：编译失败回退逐 form（宽松语义）=====
;; per-form 路径展开/求值交错，flag 在展开期可见；回退保持了脚本语义。
(define src5 (string-append (os-temp-dir) "/gf-ewx-5.scm"))
(define out5 (string-append (os-temp-dir) "/gf-ewx-5.out"))
(call-with-output-file src5
  (lambda (p)
    (display "(import (goldfish))" p) (newline p)
    (display "(define flag #f)" p) (newline p)
    (display "(eval-when (expand) (set! flag #t))" p) (newline p)
    (display (string-append "(call-with-output-file \"" out5
                            "\" (lambda (p) (write flag p)))") p)
    (newline p)))
(when (file-exists? out5) (delete-file out5))
(clear-artifact! src5)
(load src5)
(check-true (file-exists? out5))
(check (call-with-input-file out5 read) => #t)
(delete-file src5)
(when (file-exists? out5) (delete-file out5))

;; ===== 6. 嵌套区域：begin-for-syntax 内的 eval-when (expand) =====
;; 内层 region define 进专用 region 库（phase+1 可见），外层 program
;; 的后续 transformer 体经 phase 门控解析到。
(define src6
  (write-program "ewx-6"
    "(import (goldfish))\n"
    "(begin-for-syntax (eval-when (expand) (define (h2 x) (* x 5))))\n"
    "(define-syntax m2\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 2 (unsyntax (h2 4))))))))\n"
    "(define value (m2))\n"))
(let* ((opt (begin (clear-artifact! src6)
                   (compile-file-cached src6)))
       (datum (syntax->datum opt)))
  (check-true (datum-contains? datum 40)))
(delete-file src6)

;; ===== 7. 同秒同大小改写：内容哈希戳防陈旧命中 =====
;; mtime 是秒级：单字符改写（同字节长度、同一秒内）曾会陈旧命中旧缓存。
;; gfo-stamp 现含内容 md5，改写后必须重编译出新值。
(define src7 (write-program "ewx-7" "(import (goldfish))\n(define v 1)\n"))
(clear-artifact! src7)
(check (datum-contains? (syntax->datum (compile-file-cached src7)) 1) => #t)
;; 同字节长度改写：1 -> 2
(write-program "ewx-7" "(import (goldfish))\n(define v 2)\n")
(let ((datum (syntax->datum (compile-file-cached src7))))
  (check (datum-contains? datum 2) => #t)
  (check (datum-contains? datum 1) => #f))
(delete-file src7)

(check-report)
