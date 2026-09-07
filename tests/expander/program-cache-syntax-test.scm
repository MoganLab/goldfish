(import (liii check) (liii os) (goldfish))

;; 程序缓存：bundle schema v1。
;;
;; 所有展开期产物共用一个 bundle 记录 (bundle <version> <kind> <section>*)。
;; program 类携带 (exprs s)：quasisyntax 子模板在值位置产出的 syntax 常量
;; （(quote-syntax X) 形式路径）写缓存前经 serialize-cache-sexp 降级为
;; stx* 纯文本，读回经 deserialize-cache-sexp 重建活记录。序列化器是
;; 可持久化内容的唯一裁判：活过程/外来记录直接 raise，产物拿不到缓存。
;;
;; 测试在 GOLDFISH_CACHE_READONLY=1 下也要能跑（goldtest runner 的约定），
;; 所以不走 gfo-write!：手工按 schema 组装 .gfo 记录并写文件，用真实的
;; compile-file-cached 冷/热路径验证读回命中逻辑。

(define serialize (module-ref the-expander-library 'serialize-cache-sexp))
(define gfo-version (module-ref the-expander-library 'gfo-format-version))
(define deserialize (module-ref the-expander-library 'deserialize-cache-sexp))

;; 产物是纯文本：完整 reader 直接可读（有 #g/标签则这里先炸）。
(define (read-record artifact)
  (call-with-input-file artifact read))

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

;; 载荷不含记录（stx* 文本域的约束）。
(define (contains-record? v) (tree-contains? record-instance? v))

;; 收集一个展开产物里的全部 syntax 记录（含重建后的）。
(define (find-syntax v)
  (let ((seen '()) (acc '()))
    (let walk ((v v))
      (cond
        ((assq v seen) #f)
        (else
          (set! seen (cons (cons v #t) seen))
          (cond
            ((syntax? v) (set! acc (cons v acc)) #f)
            ((pair? v) (or (walk (car v)) (walk (cdr v))))
            ((and (vector? v) (not (bytevector? v)))
             (let loop ((i 0))
               (if (< i (vector-length v))
                 (or (walk (vector-ref v i)) (loop (+ i 1)))
                 #f)))
            (else #f)))))
    (reverse acc)))

;; 序列化产物里存在 (stx* ...) 描述（语法常量已文本化）。
(define (has-stx-shape? v)
  (tree-contains? (lambda (x) (and (pair? x) (eq? (car x) 'stx*))) v))

;; compile-file-cached 按优化级别在 key-<oN>.gfo 里找缓存；把手工组装的
;; 记录写到所有候选路径，无论运行在哪一级都能命中。
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

;; ===== 1. 序列化器是唯一裁判：不可序列化值 raise =====
(check-catch 'no-catch (serialize (list 1 (lambda (x) x))))
(check (serialize (list 1 '(a b))) => '(1 (a b)))

;; ===== 2. 6a 型程序：exprs 降级为 stx*，读回重建活记录 =====
(define src6a (string-append (os-temp-dir) "/gf-prog-cache-syntax6a.scm"))
(call-with-output-file src6a
  (lambda (p)
    (display "(import (goldfish))" p) (newline p)
    (display "(define-syntax m (lambda (stx) (syntax-case stx () ((_) (quasisyntax (list 1 (syntax (lit 2))))))))" p) (newline p)
    (display "(define value (m))" p) (newline p)))

(let* ((cold (compile-file-cached src6a))
       (exprs (serialize cold))
       (rec (list 'gfo gfo-version (compile-file-stamp src6a)
                  (list 'bundle 1 'program (list 'exprs exprs))
                  '(((goldfish) . external)))))
  ;; 冷路径返回活 opt：嵌入的 syntax 常量在内存里是活记录
  (check (syntax->datum (car (find-syntax cold))) => '(lit 2))
  ;; 序列化产物是纯文本（无记录），语法常量已降级为 stx*
  (check (contains-record? exprs) => #f)
  (check-true (has-stx-shape? exprs))
  ;; 写入 schema 记录后，compile-file-cached 必须命中：
  ;; 读回重建活的 syntax 记录，datum 与冷路径一致
  (write-artifact! src6a rec)
  (let* ((warm (compile-file-cached src6a))
         (warm-stxs (find-syntax warm)))
    (check (length warm-stxs) => 1)
    (check (syntax->datum (car warm-stxs)) => '(lit 2))
    (check (syntax->datum (car (find-syntax cold)))
          => (syntax->datum (car warm-stxs))))
  ;; 读回重建的记录是活 syntax（load 的消费方式：载荷里 (quote #<stx>)
  ;; 自引用求值为记录本身）
  (check-true (syntax? (car (find-syntax (deserialize exprs))))))

;; ===== 3. 普通模板 literal：exprs 无 stx* / 活记录 =====
;; 整模板 literal（宏输出本身就是 literal）在 IR 路径已被 datum 化，
;; exprs 应是纯文本——防回归：降级路径不得影响纯文本程序。
(define src-plain (string-append (os-temp-dir) "/gf-prog-cache-plain.scm"))
(call-with-output-file src-plain
  (lambda (p)
    (display "(import (goldfish))" p) (newline p)
    (display "(define-syntax m (lambda (stx) (syntax-case stx () ((_) (syntax (a b))))))" p) (newline p)
    (display "(define value (m))" p) (newline p)))

(let* ((cold (compile-file-cached src-plain))
       (exprs (serialize cold))
       (rec (list 'gfo gfo-version (compile-file-stamp src-plain)
                  (list 'bundle 1 'program (list 'exprs exprs))
                  '(((goldfish) . external)))))
  (check-true (and (pair? exprs) (eq? (car exprs) 'define)))
  (check (contains-record? exprs) => #f)
  (check (has-stx-shape? exprs) => #f)
  ;; 热路径：读回重建与序列化前 equal?
  (write-artifact! src-plain rec)
  (check (let ((warm (compile-file-cached src-plain)))
           (equal? warm exprs))
        => #t)
  (check (find-syntax (compile-file-cached src-plain)) => '()))

(delete-file src6a)
(delete-file src-plain)
(check-report)
