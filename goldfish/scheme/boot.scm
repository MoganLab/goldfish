(define (void) (if #f #f))

(define (andmap f first . rest)
  (if (null? rest)
      (let andmap ((first first))
        (if (null? first)
            #t
            (let ((x (car first))
                  (rest (cdr first)))
              (if (null? rest)
                  (f x)
                  (and (f x) (andmap rest))))))
      (let andmap ((first first) (rest rest))
        (if (null? first)
            #t
            (let ((x (car first))
                  (xr (map car rest))
                  (next-first (cdr first))
                  (next-rest (map cdr rest)))
              (if (null? next-first)
                  (apply f (cons x xr))
                  (and (apply f (cons x xr))
                       (andmap next-first next-rest))))))))

(define (ormap f list1)
  (and (not (null? list1))
       (or (f (car list1))
           (ormap f (cdr list1)))))

(define *symbol-properties* (make-hash-table))

(define (putprop symbol key value)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-set! props key value)
        (let ((new-props (make-hash-table)))
          (hash-table-set! new-props key value)
          (hash-table-set! *symbol-properties* symbol new-props)))
    value))

(define (getprop symbol key)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-ref props key)
        #f)))

(define (remprop symbol key)
  (let ((props (hash-table-ref *symbol-properties* symbol)))
    (if props
        (hash-table-set! props key #f))
    #f))

(define s7-gensym gensym)
(define (gensym x)
  (cond
    ((symbol? x) (s7-gensym (symbol->string x)))
    (else        (s7-gensym x))))

;; API provided by psyntax
(define $sc-put-cte             #f)
(define sc-expand               #f)
(define $make-environment       #f)
(define environment?            #f)
(define interaction-environment #f)
(define identifier?             #f)
(define unwrap-syntax           #f)
(define syntax->list            #f)
(define syntax-object->datum    #f)
(define datum->syntax-object    #f)
(define generate-temporaries    #f)
(define free-identifier=?       #f)
(define bound-identifier=?      #f)
(define literal-identifier=?    #f)
(define syntax-error            #f)
(define $syntax-dispatch        #f)

(define syntax->vector          #f)

(set! (*s7* 'symbol-quote?) #t)

(define s7-append append)
(set! append
  (lambda args
    (cond
      ;; 0 或 1 个参数
      ((or (null? args) (null? (cdr args)))
       (apply s7-append args))

      ((not (proper-list? (car (last-pair args))))
       (let ((prefix (apply s7-append (drop-right args 1))))
         (if (null? prefix)
             (car (last-pair args))
             (let copy ((lst prefix))
               (if (null? (cdr lst))
                   (cons (car lst) (car (last-pair args)))
                   (cons (car lst) (copy (cdr lst))))))))

      (else (apply s7-append args)))))

(unless (defined? 'drop-right)
  (define (drop-right lst n)
    (let loop ((lst lst) (result '()))
      (if (or (null? lst) (null? (cdr lst)))
          (reverse result)
          (loop (cdr lst) (cons (car lst) result))))))

(unless (defined? 'last-pair)
  (define (last-pair lst)
    (if (null? (cdr lst))
        lst
        (last-pair (cdr lst)))))

(define %primitive-read        read)
(define %primitive-eval        eval)
(define %primitive-eval-string eval-string)
(define %primitive-load        load)

(define read        g_goldfish-read)
(define eval        g_goldfish-eval)
(define eval-string g_goldfish-eval-string)
(define load        g_goldfish-load)

(%primitive-load "scheme/psyntax.pp")

(define syntax->datum syntax-object->datum)
(define datum->syntax datum->syntax-object)




(set! *#readers*
  (append
    (list
      ;; #` (quasisyntax)
      (cons #\` (lambda (str)
                  (list 'quasisyntax (read))))
      ;; #, (unsyntax)
      (cons #\, (lambda (str)
                  ;; 检查后面是否跟着 @，即 #,@ (unsyntax-splicing)
                  (let ((next-char (peek-char)))
                    (if (eq? next-char #\@)
                        (begin
                          (read-char) ;; 消耗掉 @
                          (list 'unsyntax-splicing (read)))
                        (list 'unsyntax (read)))))))
      ;; #' (syntax)
      ;; NOTE: 不能简单这样写，见下方注释
      ; (cons #\' (lambda (str)
      ;             (list 'syntax (read)))))
    *#readers*))

;; ---------------------------------------------------------
;; 注意：在 s7 的 *#readers* hook 中，不能简单地对原子符号使用 (read)。
;; 因为此时 (read) 为了确定一个 symbol 的结束，会预读下一个字符。
;; 如果 #'foo 紧跟括号，如 (#'foo)，内部的 (read) 会消耗掉那个 ')'，
;; 导致外层解析器因丢失闭合括号而报错。
;; 如果不是预期如此，这应当是 s7 的 bug
;; ---------------------------------------------------------

(define (goldfish-read-syntax-object port)
  (define (delimiter? c)
    (or (eof-object? c)
        (char-whitespace? c)
        ;; 核心界定符：这些字符标志着一个原子符号的终结
        ;; 我们必须在读到这些字符之前停止，且绝不能从 port 中消耗它们
        (case c ((#\( #\) #\[ #\] #\{ #\} #\" #\;) #t) (else #f))))

  ;; 1. 处理前置空格（确保跳过 #' 后的空白）
  (let skip-ws ()
    (let ((c (peek-char port)))
      (if (and (not (eof-object? c)) (char-whitespace? c))
          (begin (read-char port) (skip-ws)))))

  (let ((next (peek-char port)))
    (cond
      ((eof-object? next) next)

      ;; 2. 处理复合结构如 #'(foo)
      ;; 此时直接调用 read 是安全的，因为 read 遇到配对的 ')' 停止，
      ;; 指针位置刚好符合预期。
      ((eq? next #\() (read port))

      ;; 3. 处理原子符号如 #'foo
      ;; 必须通过 peek-char 手动收集字符。一旦遇到界定符（如 ')'），
      ;; 立即停止收集并返回。这样指针就精准地停在界定符之前，
      ;; 从而避免了原生 (read) 误吞界定符导致的语法解析崩溃。
      (else
       (let collect ((chars '()))
         (let ((c (peek-char port)))
           (if (delimiter? c)
               (if (null? chars)
                   #f 
                   (string->symbol (list->string (reverse chars))))
               (collect (cons (read-char port) chars)))))))))

;; 注册宏
(set! *#readers*
  (cons
    (cons #\' (lambda (str)
                (list 'syntax (goldfish-read-syntax-object (current-input-port)))))
    *#readers*))

(set-expander!
  (lambda (expr)
    (sc-expand expr #f '(E) '(E))))

(load "scheme/s7-shim.scm")
(load "scheme/r7rs-small.scm")

; (load "demo/demo_psyntax.scm")
