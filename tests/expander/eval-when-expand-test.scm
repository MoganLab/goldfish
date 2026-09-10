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

;; ===== 5. 展开期 set! 运行期变量：编译期拒绝（v5 严格相位） =====
;; 展开区域看不到本程序的 phase-0 值绑定：set! 在展开期即报
;; unbound-variable，程序从不执行。此前整编译与 per-form 两条路径
;; 的产物分歧（#f/#t 翻转）随编译期拒绝一并消失。
(define src5 (string-append (os-temp-dir) "/gf-ewx-5.scm"))
(call-with-output-file src5
  (lambda (p)
    (display "(import (goldfish))" p) (newline p)
    (display "(define flag #f)" p) (newline p)
    (display "(eval-when (expand) (set! flag #t))" p) (newline p)))
(check-catch 'unbound-variable (compile-file-cached src5))
(delete-file src5)

;; ===== 6. 嵌套区域：begin-for-syntax 助手对外层 transformer 可见 =====
;; h2 定义于 begin-for-syntax（store[1]），m2 的体在 phase 1 展开，
;; 精确命中 store[1]。Racket 规范形：区域形式不叠加（内层再包
;; eval-when 会落到 store[2]，对 phase-1 展开不可见）。
(define src6
  (write-program "ewx-6"
    "(import (goldfish))\n"
    "(begin-for-syntax (define (h2 x) (* x 5)))\n"
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

;; ===== 8. 编译单元隔离：同名区域 define 不跨单元串值 =====
;; file1 -> file2 -> 重编译 file1：各单元在自己的 expand 环境求值，
;; 同名 helper 互不覆盖，重编译仍折叠 file1 自己的值（42，而非 file2 的 120）。
(define src8a
  (write-program "ewx-8a"
    "(import (goldfish))\n"
    "(eval-when (expand) (define (helper x) (+ x 1)))\n"
    "(define-syntax m\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 2 (unsyntax (helper 20))))))))\n"
    "(define value (m))\n"))
(define src8b
  (write-program "ewx-8b"
    "(import (goldfish))\n"
    "(eval-when (expand) (define (helper x) (* x 10)))\n"
    "(define-syntax k\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 1 (unsyntax (helper 12))))))))\n"
    "(define value (k))\n"))
(clear-artifact! src8a)
(check (datum-contains? (syntax->datum (compile-file-cached src8a)) 42) => #t)
(clear-artifact! src8b)
(check (datum-contains? (syntax->datum (compile-file-cached src8b)) 120) => #t)
;; 重编译 file1（源未变，产物已清）：折叠回 file1 自己的 helper
(check (datum-contains? (syntax->datum (compile-file-cached src8a)) 42) => #t)
(check (datum-contains? (syntax->datum (compile-file-cached src8a)) 120) => #f)
(delete-file src8a)
(delete-file src8b)

;; ===== 9. 编译单元隔离：跨文件 phase+1 引用不可见 =====
;; file1 的区域 define 只活在 file1 的单元里；同进程后编译的 file2
;; 在 transformer 体里引用它必须 unbound-variable（旧共享 region 库
;; 的实现会泄漏解析成功）。
(define src9a
  (write-program "ewx-9a"
    "(import (goldfish))\n"
    "(eval-when (expand) (define secret 99))\n"
    "(define value 1)\n"))
(define src9b
  (write-program "ewx-9b"
    "(import (goldfish))\n"
    "(define-syntax m\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (unsyntax secret))))))\n"
    "(define value (m))\n"))
(clear-artifact! src9a)
(clear-artifact! src9b)
(compile-file-cached src9a)
(check-catch 'unbound-variable (compile-file-cached src9b))
(delete-file src9a)
(delete-file src9b)

;; ===== 10. 任意相位：嵌套 begin-for-syntax 上的 phase-2 机制 =====
;; 外层 begin-for-syntax 的上下文是 phase 1，其内层再 +1：deep 进
;; store[2]，inner 的 RHS 在 phase 2 展开精确命中（21）。同名的
;; phase-1 deep（store[1]）按精确相位优先：ph1use 折出 200，互不串值。
;; （程序为 tests/expander/resources/ 下的真实文件，避免字符串括号
;; 计数；compile-fresh = 全新严格程序库，各用例互不串染。）
(define (compile-fresh src)
  (compile-file-into src (make-program-library)))
(let ((datum (syntax->datum
              (compile-fresh "tests/expander/resources/ewx-phase2.scm"))))
  (check (datum-contains? datum 21) => #t)
  (check (datum-contains? datum 200) => #t))

;; ===== 11. 无界深度：内层 bfs 的助手服务于 phase-2 宏 RHS =====
;; at2 注册于 phase 1（嵌套 bfs 内的 define-syntax），其 RHS 在
;; phase 2 展开：deep 精确命中 store[2]——(deep 10) = 70 折进产物。
(check (datum-contains?
         (syntax->datum
           (compile-fresh "tests/expander/resources/ewx-cross.scm"))
         70)
       => #t)

;; ===== 12. v5 严格相位：transformer 体不可见本程序 phase-0 值 =====
;; 单一规则、两条路径同断言：program 与 library 的 capture 都在
;; 展开期报 unbound-variable。region 助手（case 1/6）与 substrate
;; 导入（一切 lambda 宏的 syntax API）不受影响。
(define src12
  (write-program "ewx-strict-program"
    "(import (goldfish))\n"
    "(define (runtime-helper x) (* x 2))\n"
    "(define-syntax m12\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 1 (unsyntax (runtime-helper 3))))))))\n"
    "(define value (m12))\n"))
(check-catch 'unbound-variable (compile-file-cached src12))
(delete-file src12)

(define src12-lib
  (write-program "ewx-strict-lib"
    "(define-library (ewx strict-lib)\n"
    "  (import (goldfish))\n"
    "  (export v)\n"
    "  (begin\n"
    "    (define (runtime-helper x) (* x 2))\n"
    "    (define-syntax m12\n"
    "      (lambda (stx)\n"
    "        (syntax-case stx ()\n"
    "          ((_) (quasisyntax (* 1 (unsyntax (runtime-helper 3))))))))\n"
    "    (define value (m12))))\n"))
(check-catch 'unbound-variable (compile-file-cached src12-lib))
(delete-file src12-lib)

;; ===== 13. v5 合法来路：region 助手（phase 精确）与 substrate 导入 =====
;; region 定义的助手在 phase 1 可用（折叠 8）；经 plain 导入的
;; (goldfish) 运行时 API（make-fresh-name）在任意相位可用。
(define src13
  (write-program "ewx-legal-helpers"
    "(import (goldfish))\n"
    "(eval-when (expand) (define (region-helper x) (+ x 3)))\n"
    "(define-syntax m13\n"
    "  (lambda (stx)\n"
    "    (syntax-case stx ()\n"
    "      ((_) (quasisyntax (* 1 (unsyntax (region-helper 5))))))))\n"
    "(define value (m13))\n"))
(check (datum-contains? (syntax->datum (compile-file-cached src13)) 8) => #t)
(delete-file src13)

(check-report)
