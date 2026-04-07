(define-library (liii goldfix-paren-right-tag-plan)
  (import (scheme base))
  (import (liii goldfix-env))
  (import (liii goldfix-lint))
  (import (liii ascii))
  (import (liii goldfix-line))
  (import (liii goldfix-paren-core))
  (import (liii list))

  (export sort-envs-for-insertion)
  (export env-needs-right-tag?)
  (export detail-needs-rewrite?)
  (export env-in-prefixed-context?)
  (export detail-needs-structural-insert?)
  (export sort-details-for-structural-insert)

  (begin
    (define (line-is-right-tag? line env)
      (string=? line (make-right-tag-line env))
    ) ;define

    (define (env-needs-right-tag? env lines)
      (let ((rline (env-rparen-line env))
            (lline (env-lparen-line env)))
        (cond
          ((not rline) #t)
          ((= rline lline) #f)
          (else
           (not (line-is-right-tag? (list-ref lines (- rline 1)) env))
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (collect-envs-needing-right-tags envs lines)
      (let loop ((rest envs) (result '()))
        (if (null? rest)
          (reverse result)
          (loop (cdr rest)
                (if (env-needs-right-tag? (car rest) lines)
                  (cons (car rest) result)
                  result
                ) ;if
          ) ;loop
        ) ;if
      ) ;let
    ) ;define

    (define (split-list lst)
      (let loop ((rest lst) (left '()) (right '()) (send-left? #t))
        (if (null? rest)
          (values (reverse left) (reverse right))
          (if send-left?
            (loop (cdr rest) (cons (car rest) left) right #f)
            (loop (cdr rest) left (cons (car rest) right) #t)
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (merge-sorted-lists left right less?)
      (cond
        ((null? left) right)
        ((null? right) left)
        ((not (less? (car right) (car left)))
         (cons (car left)
               (merge-sorted-lists (cdr left) right less?)
         ) ;cons
        ) ;
        (else
         (cons (car right)
               (merge-sorted-lists left (cdr right) less?)
         ) ;cons
        ) ;else
      ) ;cond
    ) ;define

    (define (merge-sort-list lst less?)
      (if (or (null? lst) (null? (cdr lst)))
        lst
        (let-values (((left right) (split-list lst)))
          (merge-sorted-lists (merge-sort-list left less?)
                              (merge-sort-list right less?)
                              less?
          ) ;merge-sorted-lists
        ) ;let-values
      ) ;if
    ) ;define

    (define (sort-envs-for-insertion envs lines)
      (let ((missing (collect-envs-needing-right-tags envs lines)))
        (define (has-next-sibling? env)
          (find-next-sibling env envs)
        ) ;define
        (merge-sort-list
          missing
          (lambda (a b)
            (let ((a-has-next (has-next-sibling? a))
                  (b-has-next (has-next-sibling? b)))
              (cond
                ((and (not a-has-next) b-has-next) #t)
                ((and a-has-next (not b-has-next)) #f)
                ((and a-has-next b-has-next)
                 (or (> (env-lparen-col a) (env-lparen-col b))
                     (and (= (env-lparen-col a) (env-lparen-col b))
                          (> (env-lparen-line a) (env-lparen-line b))
                     ) ;and
                 ) ;or
                ) ;
                (else
                 (< (env-lparen-col a) (env-lparen-col b))
                ) ;else
              ) ;cond
            ) ;let
          ) ;lambda
        ) ;merge-sort-list
      ) ;let
    ) ;define

    (define (detail-needs-rewrite? detail current-lines)
      (let ((explicit-line (env-detail-explicit-rparen-line detail)))
        (and explicit-line
             (let* ((env (env-detail-env detail))
                    (expected-line (make-right-tag-line env))
                    (current-line (list-ref current-lines (- explicit-line 1))))
               (not (string=? current-line expected-line))
             ) ;let*
        ) ;and
      ) ;let
    ) ;define

    (define (line-opened-with-prefix? line col)
      (let ((len (string-length line)))
        (let loop ((i 0) (prefixed? #f))
          (if (>= i len)
            #f
            (let ((ch (string-ref line i)))
              (cond
                ((char-whitespace? ch)
                 (loop (+ i 1) prefixed?)
                ) ;
                ((and (= i col) (ascii-left-paren? ch))
                 prefixed?
                ) ;
                ((char=? ch #\')
                 (loop (+ i 1) #t)
                ) ;
                ((char=? ch #\`)
                 (loop (+ i 1) #t)
                ) ;
                ((char=? ch #\,)
                 (if (and (< (+ i 1) len)
                          (char=? (string-ref line (+ i 1)) #\@))
                   (loop (+ i 2) #t)
                   (loop (+ i 1) #t)
                 ) ;if
                ) ;
                (else
                 #f
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (env-in-prefixed-context? env lines)
      (let loop ((current env))
        (if (not current)
          #f
          (let ((line (list-ref lines (- (env-lparen-line current) 1))))
            (if (line-opened-with-prefix? line (env-lparen-col current))
              #t
              (loop (env-parent current))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (line-net-close-count lines target-line)
      (let loop ((remaining lines)
                 (line-num 1)
                 (block-depth 0)
                 (in-string #f)
                 (escape-next #f))
        (if (null? remaining)
          0
          (let-values (((paren-counts next-block-depth next-in-string next-escape-next)
                        (count-parens-with-state (car remaining)
                                                 block-depth
                                                 in-string
                                                 escape-next))
                        ) ;count-parens-with-state
            (let ((lparen-count (car paren-counts))
                  (rparen-count (cdr paren-counts)))
              (if (= line-num target-line)
                (max 0 (- rparen-count lparen-count))
                (loop (cdr remaining)
                      (+ line-num 1)
                      next-block-depth
                      next-in-string
                      next-escape-next
                ) ;loop
              ) ;if
            ) ;let
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    (define (env-depth env)
      (let loop ((current env) (depth 0))
        (if (not current)
          depth
          (loop (env-parent current) (+ depth 1))
        ) ;if
      ) ;let
    ) ;define

    (define (details-closing-at-line details target-line)
      (filter (lambda (detail)
                (let* ((env (env-detail-env detail))
                       (close-line (env-detail-close-line detail))
                       (lparen-line (env-lparen-line env)))
                  (and close-line
                       (= close-line target-line)
                       (> close-line lparen-line)
                  ) ;and
                ) ;let*
              ) ;
              details
      ) ;filter
    ) ;define

    (define (sort-details-for-actual-close details)
      (merge-sort-list
        details
        (lambda (a b)
          (let* ((a-env (env-detail-env a))
                 (b-env (env-detail-env b))
                 (a-depth (env-depth a-env))
                 (b-depth (env-depth b-env)))
            (or (> a-depth b-depth)
                (and (= a-depth b-depth)
                     (or (> (env-lparen-line a-env) (env-lparen-line b-env))
                         (and (= (env-lparen-line a-env) (env-lparen-line b-env))
                              (> (env-lparen-col a-env) (env-lparen-col b-env))
                         ) ;and
                     ) ;or
                ) ;and
            ) ;or
          ) ;let*
        ) ;lambda
      ) ;merge-sort-list
    ) ;define

    (define (detail-selected-for-structural-insert? detail lines details)
      (let* ((close-line (env-detail-close-line detail))
             (available (line-net-close-count lines close-line))
             (closing-details (sort-details-for-actual-close
                                (details-closing-at-line details close-line)))
             ) ;closing-details
        (let loop ((rest closing-details) (remaining available))
          (if (or (null? rest) (<= remaining 0))
            #f
            (if (eq? (car rest) detail)
              #t
              (loop (cdr rest) (- remaining 1))
            ) ;if
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (detail-needs-structural-insert? detail lines details)
      (let* ((env (env-detail-env detail))
             (close-line (env-detail-close-line detail))
             (explicit-line (env-detail-explicit-rparen-line detail))
             (lparen-line (env-lparen-line env))
             (close-line-text (and close-line
                                   (list-ref lines (- close-line 1)))
             ) ;close-line-text
             (net-close-count (and close-line
                                   (line-net-close-count lines close-line))
             ) ;net-close-count
             (trailing-unmatched-count (and close-line-text
                                            (line-trailing-unmatched-rparen-count close-line-text)))
             ) ;trailing-unmatched-count
        (and close-line
             (> close-line lparen-line)
             (not explicit-line)
             (not (env-in-prefixed-context? env lines))
             (> net-close-count 0)
             (>= trailing-unmatched-count net-close-count)
             (detail-selected-for-structural-insert? detail lines details)
        ) ;and
      ) ;let*
    ) ;define

    (define (env-ancestor-of? maybe-ancestor env)
      (let loop ((current (and env (env-parent env))))
        (if (not current)
          #f
          (if (eq? current maybe-ancestor)
            #t
            (loop (env-parent current))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (sort-details-for-structural-insert details)
      (merge-sort-list
        details
        (lambda (a b)
          (let* ((a-env (env-detail-env a))
                 (b-env (env-detail-env b))
                 (a-close (env-detail-close-line a))
                 (b-close (env-detail-close-line b))
                 (a-depth (env-depth a-env))
                 (b-depth (env-depth b-env)))
            (or (> a-close b-close)
                (and (= a-close b-close)
                     (cond
                       ((env-ancestor-of? a-env b-env) #t)
                       ((env-ancestor-of? b-env a-env) #f)
                       ((< a-depth b-depth) #t)
                       ((> a-depth b-depth) #f)
                       ((< (env-lparen-line a-env) (env-lparen-line b-env)) #t)
                       ((> (env-lparen-line a-env) (env-lparen-line b-env)) #f)
                       (else
                        (< (env-lparen-col a-env) (env-lparen-col b-env))
                       ) ;else
                     ) ;cond
                ) ;and
            ) ;or
          ) ;let*
        ) ;lambda
      ) ;merge-sort-list
    ) ;define

  ) ;begin
) ;define-library
