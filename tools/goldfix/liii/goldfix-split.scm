(define-library (liii goldfix-split)
  (import (scheme base))
  (import (liii string))
  (import (liii goldfix-constant))
  (import (liii goldfix-env))
  (import (liii goldfix-lint))
  (import (liii goldfix-line-scan))
  (import (liii goldfix-scheme))

  (export normalize-multi-open-lines)

  (begin
    (define (let-family-tag? tag)
      (or (string=? tag "let")
          (string=? tag "let*")
          (string=? tag "letrec")
          (string=? tag "letrec*")
      ) ;or
    ) ;define

    (define (prepass-forbidden-tag? tag)
      (or (string-null? tag)
          (string=? tag "if")
          (string=? tag "cond")
          (string=? tag "and")
          (string=? tag "or")
          (string=? tag "lambda")
          (string=? tag "let")
          (string=? tag "let*")
          (string=? tag "letrec")
          (string=? tag "letrec*")
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

    (define (nested-lambda-parent-tag? tag)
      (or (string=? tag "map")
          (string=? tag "filter")
      ) ;or
    ) ;define

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

    (define (line-starts-in-code? block-depth in-string escape-next)
      (and (= block-depth 0)
           (not in-string)
           (not escape-next)
      ) ;and
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

    (define (shift-line-indentation line delta)
      (let* ((len (string-length line))
             (current-indent (line-leading-space-count line))
             (trimmed (substring line current-indent len)))
        (if (string-null? trimmed)
          ""
          (let ((new-indent (max 0 (+ current-indent delta))))
            (string-append (make-string new-indent #\space) trimmed)
          ) ;let
        ) ;if
      ) ;let*
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

    (define (split-plan-lines plan)
      (car plan)
    ) ;define

    (define (split-plan-start-index plan)
      (cadr plan)
    ) ;define

    (define (split-plan-indent-delta plan)
      (caddr plan)
    ) ;define

    (define (split-plan-scope plan)
      (cadddr plan)
    ) ;define

    (define (binding-plan-from-open-index line binding-open-index)
      (let ((tag-range (open-form-tag-range line binding-open-index)))
        (if (not tag-range)
          #f
          (let* ((binding-tag (substring line (car tag-range) (cdr tag-range)))
                 (value-start (direct-child-open-index line (cdr tag-range))))
            (if (or (prepass-forbidden-tag? binding-tag)
                    (not value-start)
                    (prepass-forbidden-tag?
                      (extract-tag (substring line value-start (string-length line)))
                    ) ;prepass-forbidden-tag?
                    (not (form-continues-after-line? line value-start))
                ) ;or
              #f
              (make-split-plan line value-start (+ binding-open-index 2) 'child)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (binding-value-split-plan env line)
      (let ((parent (env-parent env)))
        (and parent
             (let-family-tag? (env-tag parent))
             (line-comment-free? line)
             (binding-plan-from-open-index line (env-lparen-col env))
        ) ;and
      ) ;let
    ) ;define

    (define (leading-let-binding-split-plan env line)
      (if (or (not (let-family-tag? (env-tag env)))
              (not (line-comment-free? line)))
        #f
        (let ((head-end (env-head-end-index line env)))
          (if (not head-end)
            #f
            (let ((binding-list-open (direct-child-open-index line head-end)))
              (if (not binding-list-open)
                #f
                (let ((binding-open (direct-child-open-index line (+ binding-list-open 1))))
                  (if binding-open
                    (binding-plan-from-open-index line binding-open)
                    #f
                  ) ;if
                ) ;let
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (collect-binding-split-plan env line _remaining-lines)
      (or (binding-value-split-plan env line)
          (leading-let-binding-split-plan env line)
      ) ;or
    ) ;define

    (define (nested-lambda-split-plan env line _remaining-lines)
      (if (or (string-null? (env-tag env))
              (not (nested-lambda-parent-tag? (env-tag env)))
              (not (line-comment-free? line)))
        #f
        (let ((head-end (env-head-end-index line env)))
          (if (not head-end)
            #f
            (let ((child-start (direct-child-open-index line head-end)))
              (if (and child-start
                       (string=? (extract-tag
                                   (substring line child-start (string-length line)))
                                 "lambda"
                       ) ;string=?
                       (form-continues-after-line? line child-start))
                (make-split-plan line child-start (+ (env-lparen-col env) 2) 'env)
                #f
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
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
                          (> next-depth 0))
                     #f
                    ) ;
                    ((and (> depth 0)
                          (= next-depth 0))
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
                    ) ;
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

    (define (env-spans-multiple-lines? env)
      (or (not (env-rparen-line env))
          (> (env-rparen-line env) (env-lparen-line env))
      ) ;or
    ) ;define

    (define (env-children-satisfy? env pred)
      (let loop ((rest (env-children env)))
        (and (pair? rest)
             (or (pred (car rest))
                 (loop (cdr rest))
             ) ;or
        ) ;and
      ) ;let
    ) ;define

    (define (line-at-offset lines offset)
      (if (null? lines)
        #f
        (if (= offset 0)
          (car lines)
          (line-at-offset (cdr lines) (- offset 1))
        ) ;if
      ) ;if
    ) ;define

    (define (binding-child-needs-header-split? env remaining-lines binding-list-open child)
      (let* ((offset (- (env-lparen-line child) (env-lparen-line env)))
             (child-line (line-at-offset remaining-lines offset)))
        (and child-line
             (> (env-lparen-line child) (env-lparen-line env))
             (> (env-lparen-col child) binding-list-open)
             (env-spans-multiple-lines? child)
        ) ;and
      ) ;let*
    ) ;define

    (define (binding-list-header-split-plan env line remaining-lines)
      (if (or (not (let-family-tag? (env-tag env)))
              (not (line-comment-free? line)))
        #f
        (let ((head-end (env-head-end-index line env)))
          (if (not head-end)
            #f
            (let ((binding-list-open (direct-child-open-index line head-end)))
              (if (not binding-list-open)
                #f
                (if (env-children-satisfy?
                      env
                      (lambda (child)
                        (binding-child-needs-header-split?
                          env
                          remaining-lines
                          binding-list-open
                          child
                        ) ;binding-child-needs-header-split?
                      ) ;lambda
                    ) ;env-children-satisfy?
                  (make-split-plan
                    line
                    binding-list-open
                    (+ (env-lparen-col env) 2)
                    'child
                  ) ;make-split-plan
                  #f
                ) ;if
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;if
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
              (body-open
                (and clause-open
                     (clause-body-open-index line clause-open)
                ) ;and
              ) ;body-open
             ) ;
             (and body-open
                  (make-split-plan line body-open (+ clause-open 1) 'child)
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

    (define (subtree-follow-line-count lines start-index)
      (let-values (((counts block-depth in-string escape-next)
                    (count-parens-with-state
                      (substring (car lines) start-index (string-length (car lines)))
                      0
                      #f
                      #f
                    ) ;count-parens-with-state
                    )) ;counts
        (let loop ((rest (cdr lines))
                   (count 0)
                   (balance (- (car counts) (cdr counts)))
                   (block-depth block-depth)
                   (in-string in-string)
                   (escape-next escape-next))
          (cond
            ((<= balance 0)
             count
            ) ;
            ((null? rest)
             count
            ) ;
            (else
             (let-values (((next-counts next-block-depth next-in-string next-escape-next)
                           (count-parens-with-state
                             (car rest)
                             block-depth
                             in-string
                             escape-next
                           ) ;count-parens-with-state
                           )) ;counts
               (loop (cdr rest)
                     (+ count 1)
                     (+ balance (- (car next-counts) (cdr next-counts)))
                     next-block-depth
                     next-in-string
                     next-escape-next
               ) ;loop
             ) ;let-values
            ) ;else
          ) ;cond
        ) ;let
      ) ;let-values
    ) ;define

    (define (plan-follow-line-count plan env remaining-lines)
      (if (eq? (split-plan-scope plan) 'env)
        (subtree-follow-line-count remaining-lines (env-lparen-col env))
        (subtree-follow-line-count remaining-lines (split-plan-start-index plan))
      ) ;if
    ) ;define

    (define (take-lines lines count)
      (let loop ((remaining lines)
                 (needed count)
                 (taken '()))
        (if (or (= needed 0) (null? remaining))
          (values (reverse taken) remaining)
          (loop (cdr remaining)
                (- needed 1)
                (cons (car remaining) taken)
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define (advance-lex-state-through-lines lines block-depth in-string escape-next)
      (if (null? lines)
        (values block-depth in-string escape-next)
        (let-values (((_counts next-block-depth next-in-string next-escape-next)
                      (count-parens-with-state
                        (car lines)
                        block-depth
                        in-string
                        escape-next
                      ) ;count-parens-with-state
                      )) ;counts
          (advance-lex-state-through-lines
            (cdr lines)
            next-block-depth
            next-in-string
            next-escape-next
          ) ;advance-lex-state-through-lines
        ) ;let-values
      ) ;if
    ) ;define

    (define (drop-envs-before-line envs next-line-num)
      (let loop ((rest envs))
        (if (and (pair? rest)
                 (< (env-lparen-line (car rest)) next-line-num))
          (loop (cdr rest))
          rest
        ) ;if
      ) ;let
    ) ;define

    (define (prepend-lines result ordered-lines)
      (let loop ((rest ordered-lines)
                 (current result))
        (if (null? rest)
          current
          (loop (cdr rest) (cons (car rest) current))
        ) ;if
      ) ;let
    ) ;define

    (define (shift-lines-indentation lines delta)
      (map
        (lambda (line)
          (shift-line-indentation line delta)
        ) ;lambda
        lines
      ) ;map
    ) ;define

    (define (apply-split-pass lines collect-plan)
      (let ((envs (scan-environments lines)))
        (let loop ((remaining-lines lines)
                   (remaining-envs envs)
                   (line-num 1)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f)
                   (result '()))
          (if (null? remaining-lines)
            (reverse result)
            (let*
              ((line (car remaining-lines))
               (line-env (and (pair? remaining-envs)
                              (= (env-lparen-line (car remaining-envs)) line-num)
                              (car remaining-envs))
               ) ;line-env
               (plan (and line-env
                          (line-starts-in-code? block-depth in-string escape-next)
                          (collect-plan line-env line remaining-lines))
               ) ;plan
              ) ;
              (if plan
                (let*
                  ((follow-count
                     (plan-follow-line-count
                       plan
                       line-env
                       remaining-lines
                     ) ;plan-follow-line-count
                   ) ;follow-count
                   (consumed-count (+ follow-count 1))
                   (next-line-num (+ line-num consumed-count))
                  ) ;
                  (let-values (((consumed-lines rest-lines)
                                (take-lines remaining-lines consumed-count)))
                    (let-values (((next-block-depth next-in-string next-escape-next)
                                  (advance-lex-state-through-lines
                                    consumed-lines
                                    block-depth
                                    in-string
                                    escape-next
                                  ) ;advance-lex-state-through-lines
                                 )
                                ) ;states
                      (let*
                        ((shifted-follow-lines
                           (shift-lines-indentation
                             (cdr consumed-lines)
                             (split-plan-indent-delta plan)
                           ) ;shift-lines-indentation
                         ) ;shifted-follow-lines
                         (emitted-lines
                           (append (split-plan-lines plan)
                                   shifted-follow-lines
                           ) ;append
                         ) ;emitted-lines
                         (next-envs
                           (drop-envs-before-line remaining-envs next-line-num)
                         ) ;next-envs
                        ) ;
                        (loop rest-lines
                              next-envs
                              next-line-num
                              next-block-depth
                              next-in-string
                              next-escape-next
                              (prepend-lines result emitted-lines)
                        ) ;loop
                      ) ;let*
                    ) ;let-values
                  ) ;let-values
                ) ;let*
                (let-values (((_counts next-block-depth next-in-string next-escape-next)
                              (count-parens-with-state
                                line
                                block-depth
                                in-string
                                escape-next
                              ) ;count-parens-with-state
                              )) ;counts
                  (loop (cdr remaining-lines)
                        (drop-envs-before-line remaining-envs (+ line-num 1))
                        (+ line-num 1)
                        next-block-depth
                        next-in-string
                        next-escape-next
                        (cons line result)
                  ) ;loop
                ) ;let-values
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (normalize-multi-open-lines lines)
      (let loop ((current-lines lines)
                 (remaining-rounds 4))
        (let*
          ((binding-split-result
             (apply-split-pass current-lines collect-binding-split-plan))
           (nested-split-result
             (apply-split-pass binding-split-result nested-lambda-split-plan)
           ) ;nested-split-result
           (case-head-split-result
             (apply-split-pass
               nested-split-result
               case-inline-clause-split-plan
             ) ;apply-split-pass
           ) ;case-head-split-result
           (conditional-clause-split-result
             (apply-split-pass
               case-head-split-result
               conditional-clause-split-plan
             ) ;apply-split-pass
           ) ;conditional-clause-split-result
           (binding-list-header-split-result
             (apply-split-pass
               conditional-clause-split-result
               binding-list-header-split-plan
             ) ;apply-split-pass
           ) ;binding-list-header-split-result
          ) ;
          (if (or (= remaining-rounds 1)
                  (equal? binding-list-header-split-result current-lines))
            binding-list-header-split-result
            (loop binding-list-header-split-result (- remaining-rounds 1))
          ) ;if
        ) ;let*
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
