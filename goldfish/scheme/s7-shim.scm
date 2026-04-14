;; define-macro shim: converts s7's define-macro to psyntax syntax-case
;;
;; 策略：
;; 1. 在编译期用 %primitive-eval 创建 transformer 函数
;; 2. 将 transformer 存入全局 hash-table，key 为生成的唯一符号
;; 3. 生成的 define-syntax 通过 hash-table 查找来获取 transformer
;;
(define *macro-transformers* (make-hash-table))

(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      ((kw . _)
       (let ((form (syntax->datum x)))
         ;; form = (define-macro (name . args) body ...) or (define-macro name expr)
         (if (and (pair? (cadr form))
                  (symbol? (caadr form)))
             ;; 带参数的宏: (define-macro (name . args) body ...)
             (let* ((name-sym (caadr form))
                    (args-dat (cdadr form))
                    (body-dat (cddr form))
                    (transformer-proc
                      (%primitive-eval (cons 'lambda (cons args-dat body-dat))))
                    (key (symbol->string (gensym "macro-")))
                    (dummy (hash-table-set! *macro-transformers* key transformer-proc))
                    (name-id (datum->syntax (syntax kw) name-sym)))
               (with-syntax ((the-name name-id)
                             (the-key key))
                 (syntax
                   (define-syntax the-name
                     (lambda (stx)
                       (syntax-case stx ()
                         ((k . rest)
                          (datum->syntax (syntax k)
                            (apply (hash-table-ref *macro-transformers* the-key)
                                   (syntax->datum (syntax rest))))))))))))
             ;; 无参数的宏: (define-macro name expr)
             (let* ((name-sym (cadr form))
                    (expr-dat (caddr form))
                    (transformer-proc (%primitive-eval expr-dat))
                    (key (symbol->string (gensym "macro-")))
                    (dummy (hash-table-set! *macro-transformers* key transformer-proc))
                    (name-id (datum->syntax (syntax kw) name-sym)))
               (with-syntax ((the-name name-id)
                             (the-key key))
                 (syntax
                   (define-syntax the-name
                     (lambda (stx)
                       (syntax-case stx ()
                         (id (identifier? (syntax id))
                             (datum->syntax (syntax id)
                               ((hash-table-ref *macro-transformers* the-key)))))))))))))))
(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((_ . pattern) template))))))


(define-syntax-rule (lambda* (spec ...) body ...)
  (lambda*-specs (spec ...) () () body ...))

;; 归一化
(define-syntax lambda*-specs
  (syntax-rules ()
    ((_ () ((id d) ...) body ...)
     (lambda _args
       (lambda*-bind ((id d) ...) _args body ...)))
    ;; 带显式默认值
    ((_ ((x d) . rest) (acc ...) body ...)
     (lambda*-specs rest (acc ... (x d)) body ...))
    ;; 无默认值 → #f
    ((_ (x . rest) (acc ...) body ...)
     (lambda*-specs rest (acc ... (x #f)) body ...))))

;; 顺序绑定
(define-syntax lambda*-bind
  (syntax-rules ()
    ((_ () _args body ...)
     (let () body ...))
    ((_ ((x d) . more) _args body ...)
     (let ((x (if (null? _args) d (car _args)))
           (_rest (if (null? _args) '() (cdr _args))))
       (lambda*-bind more _rest body ...)))))

(define-syntax define*
  (lambda (x)
    (syntax-case x ()
      ((_ (id . args) b0 b1 ...)
       #'(define id (lambda* args (let () b0 b1 ...))))
      ((_ id val) (identifier? (syntax id))
       #'(define id val)))))
