(import (liii check) (liii os) (goldfish))

;; eval-when (expand) / begin-for-syntax：展开期区域语义。
;;
;; 区域内的 define 在展开期求值（session-local：eval-when-expand! 把
;; 定义 eval 进 the-expander-library，跨会话不持久化），同区域与后续
;; form 的 transformer 体可以直接调用它们（store@ph+1 + 按源码名的桶
;; 回退解析）。宏使用点在编译期折叠为常量，产物自包含；区域 gensym
;; 泄漏进运行期位置时，缓存层跳过缓存（tree-contains-any? 检查），
;; 文件回退逐 form 加载并保持宽松脚本语义。

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
  (check-true (datum-contains? datum 42))
  ;; 产物不含区域 gensym：缓存自包含，可跨会话
  (check (tree-contains-any? datum (expand-region-defs)) => #f))
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
  ;; 同进程先后两个 compile 各自的区域：后一区域定义同名 helper 覆盖
  ;; 桶回退，本文件折叠为 120（上一区域的 42 不应出现在本产物里）。
  (check-true (datum-contains? datum 120))
  (check (tree-contains-any? datum (expand-region-defs)) => #f))
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
  (check-true (datum-contains? datum 42))
  (check (tree-contains-any? datum (expand-region-defs)) => #f))
(delete-file src3)

;; ===== 4. 区域 define 泄漏进运行期位置：缓存跳过，会话内仍正确 =====
;; phase-0 引用经桶回退（按源码名）解析到区域 gensym：会话内可运行，
;; 但产物引用 session-local gensym —— 泄漏检查必须拦下缓存。
(define src4
  (write-program "ewx-4"
    "(import (goldfish))\n"
    "(eval-when (expand) (define hidden 5))\n"
    "(define value hidden)\n"))
(let* ((opt (begin (clear-artifact! src4)
                   (compile-file-cached src4)))
       (datum (syntax->datum opt))
       (leaked? (tree-contains-any? datum (expand-region-defs))))
  ;; phase-0 引用解析到区域 gensym：会话内可运行（区域值可见）
  (check-true (datum-contains? datum (car (expand-region-defs))))
  ;; 泄漏被检出：该文件不缓存，每次加载重新展开
  (check leaked? => #t))
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
;; 内层 core 形式的区域宿主库取自其 syntax 的 library（随外层所在
;; program/library），否则内层 define 注册进 base 库，program 里后续
;; transformer 体解析不到（曾因此整体编译失败回退逐 form）。
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
  (check-true (datum-contains? datum 40))
  (check (tree-contains-any? datum (expand-region-defs)) => #f))
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
