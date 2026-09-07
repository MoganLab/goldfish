(import (liii check) (liii os) (goldfish))

;; 宏定义程序的缓存：define-syntax / define-macro / syntax-rules 的程序与
;; 其他程序走同一条 compile-file-cached 路径（bundle schema v1）。transformer
;; 在展开期求值一次（visit 语义，与 eval-when (expand) 策略一致），产物里
;; 没有它的残留。测试在 GOLDFISH_CACHE_READONLY=1 下也能跑：手工组装
;; bundle 记录写缓存，再用真实的 (load ...) 验证命中求值。

(define serialize (module-ref the-expander-library 'serialize-cache-sexp))
(define gfo-version (module-ref the-expander-library 'gfo-format-version))

;; 深度优先谓词：pred 命中任一节点即 #t（对任意可遍历结构安全）。
(define (tree-contains? pred v)
  (let ((seen '()))
    (let walk ((v v))
      (if (assq v seen)
        #f
        (begin
          (set! seen (cons (cons v #t) seen))
          (or (pred v)
              (and (pair? v) (or (walk (car v)) (walk (cdr v))))
              (and (vector? v) (not (bytevector? v))
                   (let loop ((i 0))
                     (if (< i (vector-length v))
                       (or (walk (vector-ref v i)) (loop (+ i 1)))
                       #f)))))))))

(define (contains-record? v) (tree-contains? record-instance? v))

(define (has-stx-shape? v)
  (tree-contains? (lambda (x) (and (pair? x) (eq? (car x) 'stx*))) v))

;; 创建 FILE 的各级目录（fresh cache 下目录不存在）。
(define (ensure-parent-dir! file)
  (let loop ((i 1))
    (when (< i (string-length file))
      (when (char=? (string-ref file i) #\/)
        (let ((d (substring file 0 i)))
          (unless (file-exists? d) (mkdir d))))
      (loop (+ i 1)))))

(define (write-artifact! src rec)
  (let ((base (string-append (compile-cache-dir) "/" (cache-key-path src))))
    (for-each (lambda (suffix)
                (let ((f (string-append base suffix ".gfo")))
                  (ensure-parent-dir! f)
                  (call-with-output-file f
                    (lambda (p) (write rec p)))))
              '("" "-o1" "-o2" "-o3"))))

(define tmp (os-temp-dir))

;; 冷编译 -> 序列化干净 -> 手工写 bundle -> (load ...) 命中并求值，
;; 程序把结果写到 out 文件，测试读取比对。
(define (check-macro-program name src-text expected)
  (let* ((src (string-append tmp "/gf-macro-prog-" name ".scm"))
         (out (string-append tmp "/gf-macro-prog-" name ".out")))
    (call-with-output-file src
      (lambda (p) (display src-text p) (newline p)))
    (when (file-exists? out) (delete-file out))
    (let* ((cold (compile-file-cached src))
           (exprs (serialize cold)))
      ;; transformer 已在展开期消掉：产物是纯文本、无 stx* 残留
      (check (contains-record? exprs) => #f)
      (check (has-stx-shape? exprs) => #f)
      (write-artifact! src
        (list 'gfo gfo-version (compile-file-stamp src)
              (list 'bundle 1 'program (list 'exprs exprs))
              '(((goldfish) . external))))
      ;; 热命中：(load) 走缓存，eval 产物产生可观察输出
      (load src)
      (check-true (file-exists? out))
      (check (call-with-input-file out read) => expected))
    (delete-file src)
    (when (file-exists? out) (delete-file out))))

;; ===== 1. define-syntax（lambda transformer + syntax-case）=====
(check-macro-program
  "syntax-case"
  (string-append
    "(import (goldfish))\n"
    "(define-syntax m (lambda (stx) (syntax-case stx () ((_) #`(quote (1 (lit 2)))))))\n"
    "(call-with-output-file \""
    (string-append tmp "/gf-macro-prog-syntax-case.out")
    "\" (lambda (p) (write (syntax->datum (m)) p)))\n")
  '(1 (lit 2)))

;; ===== 2. begin 里的 syntax-rules + define-macro 混合 =====
(check-macro-program
  "begin-mix"
  (string-append
    "(import (goldfish))\n"
    "(begin\n"
    "  (define-syntax m1 (syntax-rules () ((_) '10)))\n"
    "  (define-macro (m2) `(quote 20)))\n"
    "(call-with-output-file \""
    (string-append tmp "/gf-macro-prog-begin-mix.out")
    "\" (lambda (p) (write (+ (m1) (m2)) p)))\n")
  30)

;; ===== 3. transformer 只在编译期求值一次（visit 语义）=====
;; transformer 展开期写 marker 文件：冷编译应出现一次；热命中不再重跑。
(let* ((src (string-append tmp "/gf-macro-prog-visit.scm"))
       (marker (string-append tmp "/gf-macro-prog-visit.marker"))
       (out (string-append tmp "/gf-macro-prog-visit.out"))
       (src-text (string-append
                   "(import (goldfish))\n"
                   "(define-syntax m\n"
                   "  (lambda (stx)\n"
                   "    (call-with-output-file \"" marker "\"\n"
                   "      (lambda (p) (display \"x\" p)))\n"
                   "    (syntax-case stx () ((_) #''42))))\n"
                   "(call-with-output-file \"" out "\"\n"
                   "  (lambda (p) (write (m) p)))\n")))
  (call-with-output-file src (lambda (p) (display src-text p) (newline p)))
  (when (file-exists? marker) (delete-file marker))
  (when (file-exists? out) (delete-file out))
  ;; 冷编译：transformer 求值一次，marker 出现
  (let ((cold (compile-file-cached src)))
    (check-true (file-exists? marker))
    ;; 热命中：不再重跑 transformer
    (delete-file marker)
    (write-artifact! src
      (list 'gfo gfo-version (compile-file-stamp src)
            (list 'bundle 1 'program (list 'exprs (serialize cold)))
            '(((goldfish) . external))))
    (load src)
    (check (call-with-input-file out read) => 42)
    (check (file-exists? marker) => #f))
  (delete-file src)
  (when (file-exists? marker) (delete-file marker))
  (when (file-exists? out) (delete-file out)))

(check-report)
