(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      ((_ (macro . args) body1 body ...)
       #'(define-macro macro (lambda args body1 body ...)))
      ((_ macro transformer)
       #'(define-syntax macro
            (lambda (y)
              (syntax-case y ()
                ((_ . args)
                 (let ((v (syntax->datum (syntax args))))
                   (datum->syntax #'_ (apply transformer v)))))))))))

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
