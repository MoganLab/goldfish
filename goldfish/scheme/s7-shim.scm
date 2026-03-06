(define-syntax define-macro
  (syntax-rules ()
    ;; 带参数的宏
    ((_ (name . args) body ...)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           ((_ . rest)
            (let ((transformer (lambda args body ...)))
              (datum->syntax stx
                (apply transformer (syntax->datum (syntax rest))))))))))
    ;; 无参数的宏
    ((_ name body ...)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           (id (identifier? (syntax id))
               (let ((transformer (lambda () body ...)))
                 (datum->syntax stx (transformer))))))))))

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
