(define (string-join lst . opt)
  (let ((delim (if (null? opt) " " (car opt))))
    (if (null? lst)
        ""
        (let loop ((l (cdr lst)) (acc (car lst)))
          (if (null? l)
              acc
              (loop (cdr l) (string-append acc delim (car l))))))))

(define (file-exists? path)
  (if (string? path)
    (if (not (g_access path 0)) ; F_OK
      #f
      (if (g_access path 1) ; R_OK
          #t
          (error 'permission-error (string-append "No permission: " path))))
    (error 'type-error "(file-exists? path): path should be string")))

(define (hash-table-ref/default ht key default)
  (or (hash-table-ref ht key)
      (if (procedure? default)
          (default)
          default)))

;;; ========== 辅助函数（需在宏定义前定义）==========
;; 将库名列表转换为点分隔符号，如 '(scheme base) -> 'scheme.base
(define (library-name->symbol lib-name)
  (string->symbol
   (string-join (map (lambda (x)
                       (cond ((symbol? x) (symbol->string x))
                             ((number? x) (number->string x))
                             (else (error "Invalid library name component" x))))
                     lib-name)
                ".")))

;; 库名转候选文件名列表（用于查找文件）
(define (library-name->filenames lib-name)
  (let ((base (string-join (map (lambda (x)
                                   (cond ((symbol? x) (symbol->string x))
                                         ((number? x) (number->string x))
                                         (else (error "Invalid library name component" x))))
                                lib-name)
                           "/")))
    (list (string-append base ".scm")
          (string-append base ".sld"))))

;; 文件查找（依赖 *load-path*）
(define (find-file-in-paths filename)
  (let loop ((dirs *load-path*))
    (cond ((null? dirs) #f)
          ((file-exists? (string-append (car dirs) "/" filename))
           (string-append (car dirs) "/" filename))
          (else (loop (cdr dirs))))))

;; 全局注册表：库名 -> 状态（#t 表示已加载，'loading 表示加载中）
(define *library-registry* (make-hash-table))

(define (load-library-by-name lib-name imp-stx)
  (let ((status (hash-table-ref/default *library-registry* lib-name #f)))
    (cond
      ((eq? status #t) #t)                       ; 已加载，直接返回
      ((eq? status 'loading)
       (syntax-error imp-stx
         (format #f "Circular library dependency detected: ~s" lib-name)))
      (else
       ;; 标记为加载中
       (hash-table-set! *library-registry* lib-name 'loading)
       ;; 查找文件
       (let ((candidates (library-name->filenames lib-name)))
         (let try ((files candidates))
           (if (null? files)
               (syntax-error imp-stx
                 (format #f "Library file not found: ~s (tried ~s)" lib-name candidates))
               (let ((full (find-file-in-paths (car files))))
                 (if full
                     (begin
                       (display "full: ")
                       (display full)
                       (newline)
                       (load full)                ; 调用普通 load
                       (hash-table-set! *library-registry* lib-name #t))
                     (try (cdr files)))))))))))

;; ========== define-library 宏 ==========
(define-syntax define-library
  (lambda (stx)
    (define (parse-library-decls decls exports imports body)
      (syntax-case decls (export import begin)
        (() (values exports imports body))
        (((export exp* ...) . rest)
         (parse-library-decls (syntax rest)
                              (append #'(exp* ...) exports)
                              imports
                              body))
        (((import imp* ...) . rest)
         (begin
           (for-each (lambda (imp)
                       (let ((lib-name (syntax->datum imp)))
                         (load-library-by-name lib-name imp)))
                     #'(imp* ...))
           (parse-library-decls (syntax rest)
                                exports
                                (append #'(imp* ...) imports)
                                body)))
        (((begin expr* ...) . rest)
         (parse-library-decls (syntax rest)
                              exports
                              imports
                              (append body #'(expr* ...))))))
    (define (process-exports exports)
      (let loop ((lst exports) (plain '()) (aliases '()))
        (if (null? lst)
            (values (reverse plain) (reverse aliases))
            (syntax-case (car lst) (rename)
              ((rename internal external)
               (loop (cdr lst)
                     (cons (syntax external) plain)
                     (cons #'(alias external internal) aliases)))
              (id
               (loop (cdr lst)
                     (cons (syntax id) plain)
                     aliases))))))

    (syntax-case stx ()
      ((_ name decl* ...)
       (call-with-values
         (lambda () (parse-library-decls #'(decl* ...) '() '() '()))
         (lambda (exports imports body)
           (call-with-values
             (lambda () (process-exports exports))
             (lambda (plain-exports aliases)
               (let ((lib-name (syntax->datum (syntax name)))
                     (ctx (syntax _)))
                 (let ((mid (datum->syntax ctx (library-name->symbol lib-name))))
                   (let ((imp-ids (map (lambda (imp-stx)
                                         (datum->syntax ctx
                                                        (library-name->symbol (syntax->datum imp-stx))))
                                       imports)))
                     (with-syntax ((mid              mid)
                                   ((exp-id* ...)    plain-exports)
                                   ((alias* ...)     aliases)
                                   ((imp-id* ...)    (reverse imp-ids))
                                   ((body-expr* ...) body))
                       ; (display #'(imp-id* ...)) (newline)
                       #'(module mid (exp-id* ...)
                           (%primitive-import imp-id* ...)
                           alias* ...
                           body-expr* ...)))))))))))))

(define-syntax import
  (lambda (stx)
    (define (transform-lib _ lib)
      (let ((lib-datum (syntax->datum lib)))
        ;; imperative, violation of the `map`
        (load-library-by-name lib-datum lib)

        (display "lib-datum: ")
        (display (library-name->symbol lib-datum)) ; scheme.base
        (newline)

        (datum->syntax _ (library-name->symbol lib-datum))))

    (syntax-case stx ()
      ((_ lib* ...)
       (with-syntax (((lib-symbol* ...)
                      (map (lambda (lib) (transform-lib (syntax _) lib))
                           #'(lib* ...))))
         #'(%primitive-import lib-symbol* ...))))))
