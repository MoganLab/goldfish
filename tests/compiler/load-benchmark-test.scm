(import (liii check)
        (goldfish compiler)
        (liii timeit)
        (srfi srfi-1))

;; L2-2 运行期基准：编译开/关下，库函数调用耗时对比。
;;
;; 注意：GOLDFISH_COMPILE 由进程环境决定，本测试在两个模式下
;; 分别运行，手动对比输出（bin/gf 由外部用环境变量调用）。

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
       (defs (list-ref rec 5))
       (e (inlet)))
  (for-each (lambda (d) (eval d e)) defs)
  (let ((take (gensym-name-of defs "take")))
    (define (call-in name args)
      (let ((f (eval (list 'lambda '() (cons name args)) e)))
        (f)))
    (define (bench)
      (let ((r '()))
        (do ((i 0 (+ i 1))) ((= i 20000))
          (set! r (call-in take '((list 1 2 3 4 5 6 7 8 9 10) 5))))
        r))
    (bench)
    (let ((t (timeit bench '() 50)))
      (display "take 20000x50 次: ")
      (display t) (display " s\n")
      (check (pair? (call-in take '((list 1 2 3 4 5 6 7 8 9 10) 5))) => #t))))

(check-report)
