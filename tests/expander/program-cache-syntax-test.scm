(import (liii check) (liii os) (goldfish))

;; 程序缓存：datum-embedded syntax 值的降级缓存（阶段 1）。
;;
;; quasisyntax 子模板在值位置产出的 syntax 常量（(quote-syntax X) 形式
;; 路径）带着会话 (program) 库的反指，曾让 contains-procedure? 跳过整个
;; 缓存。现在写缓存前先降级成 stx* 纯文本（(program-cache 1 <text>) 载
;; 荷，与库缓存的 macro-cache 同一文本域），读回经 deserialize-cache-sexp
;; 重建活记录。不带降级标签的载荷保持原样路径（字节不变）。

(define deserialize (module-ref the-expander-library 'deserialize-cache-sexp))

;; 产物是纯文本：完整 reader 直接可读（有 #g/标签则这里先炸）。
(define (read-record artifact)
  (call-with-input-file artifact read))

;; 载荷不含记录（stx* 文本域的约束）。
(define (contains-record? v)
  (let ((seen '()))
    (let walk ((v v))
      (cond
        ((assq v seen) #f)
        (else
          (set! seen (cons v seen))
          (cond
            ((record-instance? v) #t)
            ((pair? v) (or (walk (car v)) (walk (cdr v))))
            ((and (vector? v) (not (bytevector? v)))
             (let loop ((i 0))
               (if (< i (vector-length v))
                 (or (walk (vector-ref v i)) (loop (+ i 1)))
                 #f)))
            (else #f)))))))

(define (find-artifact src)
  (let ((base (string-append (compile-cache-dir) "/" (cache-key-path src))))
    (let loop ((suffixes '("-o1" "-o2" "-o3" "")))
      (cond
        ((null? suffixes) #f)
        ((file-exists? (string-append base (car suffixes) ".gfo"))
         (string-append base (car suffixes) ".gfo"))
        (else (loop (cdr suffixes)))))))

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

;; ===== 1. 6a 型程序：降级缓存写入 + 读回重建 =====
;; quasisyntax 子模板 (syntax (lit 2)) 作为 (list 1 ...) 的元素嵌入：
;; 曾不可缓存；现在缓存应写成功，产物是纯文本，且二次 compile 命中
;; 缓存后重建出活的 syntax 记录，语义与冷路径一致。
(define src6a (string-append (os-temp-dir) "/gf-prog-cache-syntax6a.scm"))
(call-with-output-file src6a
  (lambda (p)
    (display "(import (goldfish))" p) (newline p)
    (display "(define-syntax m (lambda (stx) (syntax-case stx () ((_) (quasisyntax (list 1 (syntax (lit 2))))))))" p) (newline p)
    (display "(define value (m))" p) (newline p)))

(let* ((cold (compile-file-cached src6a))
       (artifact (find-artifact src6a))
       (rec (and artifact (read-record artifact))))
  (check-true (string? artifact))
  ;; 冷路径返回活 opt：嵌入的 syntax 常量在内存里是活记录
  (check (syntax->datum (car (find-syntax cold))) => '(lit 2))
  ;; 产物存在且是纯文本（完整 reader 直接可读），且载荷无记录
  (check-true (and (pair? rec) (eq? (car rec) 'gfo)))
  (check (contains-record? rec) => #f)
  ;; 载荷带降级标签
  (check (let ((payload (cadddr rec)))
           (and (pair? payload) (eq? (car payload) 'program-cache)
                (equal? (cadr payload) 1)))
        => #t)
  ;; 热路径：命中缓存，读回重建活的 syntax 记录，datum 一致
  (let* ((warm (compile-file-cached src6a))
         (warm-stxs (find-syntax warm)))
    (check (length warm-stxs) => 1)
    (check (syntax->datum (car warm-stxs)) => '(lit 2))
    (check (syntax->datum (car (find-syntax cold)))
          => (syntax->datum (car warm-stxs))))
  ;; 读回重建的记录是活 syntax（load 的消费方式：载荷里 (quote #<stx>)
  ;; 自引用求值为记录本身）
  (let ((payload (cadddr (read-record artifact))))
    (check-true (syntax? (car (find-syntax
                                (deserialize (caddr payload))))))))

;; ===== 2. 普通模板 literal：原样路径不变（无降级标签） =====
;; 整模板 literal（宏输出本身就是 literal）在 IR 路径已被 datum 化，
;; 缓存走未打标的纯文本路径——防回归：降级路径不得影响它。
(define src-plain (string-append (os-temp-dir) "/gf-prog-cache-plain.scm"))
(call-with-output-file src-plain
  (lambda (p)
    (display "(import (goldfish))" p) (newline p)
    (display "(define-syntax m (lambda (stx) (syntax-case stx () ((_) (syntax (a b))))))" p) (newline p)
    (display "(define value (m))" p) (newline p)))

(let* ((cold (compile-file-cached src-plain))
       (artifact (find-artifact src-plain))
       (rec (and artifact (read-record artifact)))
       (payload (cadddr rec)))
  (check-true (string? artifact))
  (check-true (and (pair? rec) (eq? (car rec) 'gfo)))
  ;; 未打标：payload 直接就是 lowered 程序，且无 stx* / 活记录
  (check-true (and (pair? payload) (not (eq? (car payload) 'program-cache))))
  (check (contains-record? payload) => #f)
  ;; 热路径：原样返回（不重建），与文件载荷逐项一致
  (check (let ((warm (compile-file-cached src-plain)))
           (equal? warm payload))
        => #t)
  (check (find-syntax (compile-file-cached src-plain)) => '()))

(delete-file src6a)
(delete-file src-plain)
(check-report)
