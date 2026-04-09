(define-library (liii goldfix-split-cond)
  (import (scheme base))
  (import (liii string))
  (import (liii goldfix-constant))
  (import (liii goldfix-env))
  (import (liii goldfix-lint))
  (import (liii goldfix-line-scan))
  (import (liii goldfix-scheme))

  (export case-inline-clause-split-plan)
  (export conditional-clause-split-plan)

  (begin
    (define (case-family-tag? tag)
      (or (string=? tag "case")
          (string=? tag "case*")
      ) ;or
    ) ;define

    (define (conditional-parent-tag? tag)
      (or (string=? tag "cond")
          (case-family-tag? tag)
      ) ;or
    ) ;define

    (define (clause-body-forbidden-tag? tag)
      (or (string-null? tag)
          (string=? tag "if")
          (string=? tag "cond")
          (string=? tag "and")
          (string=? tag "or")
          (string=? tag "lambda")
          (string=? tag "begin")
          (string=? tag "define")
          (string=? tag "define-library")
          (string=? tag "when")
          (string=? tag "unless")
          (string=? tag "case")
          (string=? tag "case*")
          (string=? tag "guard")
          (string=? tag "do")
          (string=? tag "delay")
          (string=? tag "delay-force")
          (string=? tag "parameterize")
      ) ;or
    ) ;define

    (define (line-leading-space-count line)
      (let ((len (string-length line)))
        (let loop ((i 0))
          (if (and (< i len)
                   (char-whitespace? (string-ref line i)))
            (loop (+ i 1))
            i
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (skip-inline-whitespace line start)
      (let ((len (string-length line)))
        (let loop ((i start))
          (if (and (< i len)
                   (char-whitespace? (string-ref line i)))
            (loop (+ i 1))
            i
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (line-comment-free? line)
      (= (line-comment-start-index line) (string-length line))
    ) ;define

    (define (open-form-tag-range line open-index)
      (let* ((len (string-length line))
             (token-start (skip-inline-whitespace line (+ open-index 1))))
        (if (or (>= open-index len)
                (not (char=? (string-ref line open-index)
                             LEFT_PAREN))
                ) ;not
          #f
          (let loop ((i token-start))
            (if (and (< i len)
                     (char-identifier? (string-ref line i)))
              (loop (+ i 1))
              (if (> i token-start)
                (cons token-start i)
                #f
              ) ;if
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (env-head-end-index line env)
      (let* ((tag-range (open-form-tag-range line (env-lparen-col env)))
             (tag (env-tag env)))
        (and tag-range
             (string=? tag (substring line (car tag-range) (cdr tag-range)))
             (cdr tag-range)
        ) ;and
      ) ;let*
    ) ;define

    (define (direct-child-open-index line start)
      (let* ((child-start (skip-inline-whitespace line start))
             (len (string-length line)))
        (and (< child-start len)
             (char=? (string-ref line child-start)
                     LEFT_PAREN
             ) ;char=?
             child-start
        ) ;and
      ) ;let*
    ) ;define

    (define (form-continues-after-line? line start)
      (let-values (((counts _block-depth _in-string _escape-next)
                    (count-parens-with-state
                      (substring line start (string-length line))
                      0
                      #f
                      #f
                    ) ;count-parens-with-state
                    )) ;counts
        (> (- (car counts) (cdr counts)) 0)
      ) ;let-values
    ) ;define

    (define (skip-form-prefix-markers line start)
      (let ((len (string-length line)))
        (let loop ((i (skip-inline-whitespace line start)))
          (if (>= i len)
            i
            (let ((ch (string-ref line i)))
              (cond
                ((char=? ch #\')
                 (loop (+ i 1))
                ) ;
                ((char=? ch #\`)
                 (loop (+ i 1))
                ) ;
                ((char=? ch #\,)
                 (if (and (< (+ i 1) len)
                          (char=? (string-ref line (+ i 1)) #\@))
                   (loop (+ i 2))
                   (loop (+ i 1))
                 ) ;if
                ) ;
                (else
                 i
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (atom-form-end-index line start)
      (let ((len (string-length line)))
        (let loop ((i start))
          (if (>= i len)
            i
            (let ((ch (string-ref line i)))
              (if (or (char-whitespace? ch)
                      (char=? ch LEFT_PAREN)
                      (char=? ch RIGHT_PAREN)
                      (char=? ch #\;))
                i
                (loop (+ i 1))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (list-form-end-index line start)
      (let ((len (string-length line)))
        (let loop ((i start)
                   (depth 0)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f))
          (if (>= i len)
            #f
            (let ((ch (string-ref line i)))
              (let-values (((next-i next-block-depth next-in-string next-escape-next mode)
                            (advance-lex-state
                              line
                              i
                              block-depth
                              in-string
                              escape-next
                            ) ;advance-lex-state
                            )) ;states
                (let
                  ((next-depth
                     (if (eq? mode 'code-char)
                       (cond
                         ((char=? ch LEFT_PAREN)
                          (+ depth 1)
                         ) ;
                         ((char=? ch RIGHT_PAREN)
                          (- depth 1)
                         ) ;
                         (else
                          depth
                         ) ;else
                       ) ;cond
                       depth
                     ) ;if
                   ) ;next-depth
                  ) ;
                  (cond
                    ((and (eq? mode 'line-comment)
                          (> next-depth 0)
                     ) ;and
                     #f
                    ) ;
                    ((and (> depth 0)
                          (= next-depth 0)
                     ) ;and
                     next-i
                    ) ;
                    (else
                     (loop next-i
                           next-depth
                           next-block-depth
                           next-in-string
                           next-escape-next
                     ) ;loop
                    ) ;else
                  ) ;cond
                ) ;let
              ) ;let-values
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (form-end-index line start)
      (let* ((len (string-length line))
             (form-start (skip-form-prefix-markers line start)))
        (if (>= form-start len)
          #f
          (if (char=? (string-ref line form-start) LEFT_PAREN)
            (list-form-end-index line form-start)
            (atom-form-end-index line form-start)
          ) ;if
        ) ;if
      ) ;let*
    ) ;define

    (define (clause-body-open-index line clause-open)
      (let* ((len (string-length line))
             (first-form-start (skip-form-prefix-markers line (+ clause-open 1))))
        (if (>= first-form-start len)
          #f
          (let ((first-form-end (form-end-index line first-form-start)))
            (if (not first-form-end)
              #f
              (let ((body-start (skip-inline-whitespace line first-form-end)))
                (if (or (>= body-start len)
                        (not (char=? (string-ref line body-start)
                                     LEFT_PAREN))
                        ) ;not
                  #f
                  (let
                    ((body-tag
                       (extract-tag
                         (substring line body-start len))
                       ) ;extract-tag
                     ) ;body-tag
                    (and (not (clause-body-forbidden-tag? body-tag))
                         (form-continues-after-line? line body-start)
                         body-start
                    ) ;and
                  ) ;let
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (case-inline-clause-open-index env line)
      (let* ((len (string-length line))
             (head-end (env-head-end-index line env)))
        (if (not head-end)
          #f
          (let ((expr-start (skip-form-prefix-markers line head-end)))
            (if (>= expr-start len)
              #f
              (let ((expr-end (form-end-index line expr-start)))
                (and expr-end
                     (direct-child-open-index line expr-end)
                ) ;and
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    (define (make-split-plan line split-index indent-col scope)
      (let
        ((left-line (string-trim-right (substring line 0 split-index)))
         (right-line
           (string-trim-right
             (substring line split-index (string-length line)))
           ) ;string-trim-right
         ) ;right-line
        (and (not (string-null? left-line))
             (not (string-null? right-line))
             (list (list left-line
                         (string-append (make-string indent-col #\space)
                                        right-line
                         ) ;string-append
                   ) ;list
                   split-index
                   (- indent-col split-index)
                   scope
             ) ;list
        ) ;and
      ) ;let
    ) ;define

    (define (case-inline-clause-split-plan env line _remaining-lines)
      (if (or (not (case-family-tag? (env-tag env)))
              (not (line-comment-free? line)))
        #f
        (let ((clause-open (case-inline-clause-open-index env line)))
          (if (and clause-open
                   (clause-body-open-index line clause-open))
            (make-split-plan
              line
              clause-open
              (+ (env-lparen-col env) 2)
              'child
            ) ;make-split-plan
            #f
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (conditional-clause-env? env line)
      (let ((parent (env-parent env)))
        (and parent
             (conditional-parent-tag? (env-tag parent))
             (= (env-lparen-col env) (line-leading-space-count line))
        ) ;and
      ) ;let
    ) ;define

    (define (conditional-clause-split-plan env line _remaining-lines)
      (if (not (line-comment-free? line))
        #f
        (cond
          ((string=? (env-tag env) "cond")
           (let*
             ((head-end (env-head-end-index line env))
              (clause-open
                (and head-end
                     (direct-child-open-index line head-end)
                ) ;and
              ) ;clause-open
             ) ;
             (and clause-open
                  (form-continues-after-line? line clause-open)
                  (make-split-plan
                    line
                    clause-open
                    (+ (env-lparen-col env) 2)
                    'env-selective
                  ) ;make-split-plan
             ) ;and
           ) ;let*
          ) ;
          ((conditional-clause-env? env line)
           (let* ((clause-open (env-lparen-col env))
                  (body-open (clause-body-open-index line clause-open))
                 ) ;let*
             (and body-open
                  (make-split-plan line body-open (+ clause-open 1) 'child)
             ) ;and
           ) ;let*
          ) ;
          (else
           #f
          ) ;else
        ) ;cond
      ) ;if
    ) ;define
  ) ;begin
) ;define-library
