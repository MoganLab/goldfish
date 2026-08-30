(import (liii check)
        (goldfish)
        (goldfish compiler)
        (srfi srfi-1))

;; L2-1 系统级等价验证：对真实 expander 产物跑编译 pass 管线，
;; 折叠前后求值行为必须一致。
;;
;; 验证链路：capture-library-cache 展开 srfi-1 -> 取 low-defs（expander
;; 最终产物）-> compile-defs 应用 constant-fold + inline + simplify-if
;; -> 原/编译 defs 分别 eval 到独立 inlet -> 按同名绑定调用比较结果。
;;
;; 注意：defs 的 define 名是带 scope 后缀的 gensym（如 take:15），
;; 编译不改名，因此两个环境里同名绑定可直接调用比较。

;; find the gensym define name with the given source prefix.
(define (gensym-name-of defs prefix)
  (let ((len (string-length prefix)))
    (let loop ((ds defs))
      (if (null? ds)
        #f
        (let ((d (car ds)))
          (if (and (pair? d)
                   (eq? (car d) 'define)
                   (symbol? (cadr d))
                   (let ((s (symbol->string (cadr d))))
                     (and (>= (string-length s) len)
                          (string=? (substring s 0 len) prefix))))
            (cadr d)
            (loop (cdr ds))))))))

(let* ((forms (call-with-input-file "goldfish/srfi/srfi-1.scm" read-forms))
       (recs (let*-values (((r c) (capture-file-cache forms))) r))
       (rec (car recs))
       ;; capture-file-cache now stores record tree-il (syntax->ir/sexp), so
       ;; lift the defs back to lowered sexp for the s7 evaluator and find
       ;; define names through the toplevel-define record.
       (defs (map ir->core (list-ref rec 5))))
  (let ((compiled (compile-defs defs (list constant-fold inline simplify-if))))
    ;; 1. pass 管线可处理真实产物且结构保持
    (check (= (length compiled) (length defs)) => #t)
    ;; 2. 原/编译 defs 分别求值到独立环境
    (let ((e-orig (inlet)) (e-comp (inlet)))
      (for-each (lambda (d) (eval d e-orig)) defs)
      (for-each (lambda (d) (eval d e-comp)) compiled)
      ;; 3. 行为等价：同名绑定在两个环境中调用结果必须一致
      (define (call-in env name args)
        (let ((f (eval (list 'lambda '() (cons name args)) env)))
          (f)))
      (let ((take (gensym-name-of defs "take")))
        (check (equal? (call-in e-orig take '((list 1 2 3 4) 3))
                       (call-in e-comp take '((list 1 2 3 4) 3)))
               => #t))
      (let ((drop (gensym-name-of defs "drop")))
        (check (equal? (call-in e-orig drop '((list 1 2 3 4) 2))
                       (call-in e-comp drop '((list 1 2 3 4) 2)))
               => #t))
      (let ((append-map (gensym-name-of defs "append-map")))
        (check (equal? (call-in e-orig append-map '((lambda (x) (list x x)) (quote (1 2))))
                       (call-in e-comp append-map '((lambda (x) (list x x)) (quote (1 2)))))
               => #t))
      (let ((find (gensym-name-of defs "find")))
        (check (equal? (call-in e-orig find '((lambda (x) (> x 2)) (quote (1 2 3 4))))
                       (call-in e-comp find '((lambda (x) (> x 2)) (quote (1 2 3 4)))))
               => #t))
      ;; 4. 折叠确实发生了（至少一个 def 被改写）
      (let ((folds (let loop ((ds defs) (cs compiled) (n 0))
                     (if (null? ds)
                       n
                       (loop (cdr ds)
                             (cdr cs)
                             (+ n (if (equal? (car ds) (car cs)) 0 1)))))))
        (check (> folds 0) => #t)))))

(check-report)
