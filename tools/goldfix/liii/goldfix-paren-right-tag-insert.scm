(define-library (liii goldfix-paren-right-tag-insert)
  (import (scheme base))
  (import (liii goldfix-env))
  (import (liii goldfix-line))
  (import (liii goldfix-lint))
  (import (liii goldfix-list))
  (import (liii goldfix-paren-core))
  (import (liii goldfix-paren-right-tag-plan))
  (import (liii list))

  (export insert-single-line-of-right-tag)

  (begin
    (define (choose-next-env candidates current-envs total-lines current-lines)
      (define (safe-pos env)
        (let ((raw-pos (find-insert-position env current-envs total-lines current-lines)))
          (cond
            ((number? raw-pos) raw-pos)
            ((number? (env-lparen-line env)) (env-lparen-line env))
            (else total-lines)
          ) ;cond
        ) ;let
      ) ;define
      (define (better-env? candidate best)
        (let ((candidate-pos (safe-pos candidate))
              (best-pos (safe-pos best))
              (candidate-col (env-lparen-col candidate))
              (best-col (env-lparen-col best))
              (candidate-line (env-lparen-line candidate))
              (best-line (env-lparen-line best)))
          (or (< candidate-pos best-pos)
              (and (= candidate-pos best-pos)
                   (> candidate-col best-col)
              ) ;and
              (and (= candidate-pos best-pos)
                   (= candidate-col best-col)
                   (> candidate-line best-line)
              ) ;and
          ) ;or
        ) ;let
      ) ;define
      (let pick ((rest (cdr candidates)) (best (car candidates)))
        (if (null? rest)
          best
          (pick (cdr rest)
                (if (better-env? (car rest) best)
                  (car rest)
                  best
                ) ;if
          ) ;pick
        ) ;if
      ) ;let
    ) ;define

    (define (rewrite-right-tags current-lines details)
      (let loop ((remaining details) (updated-lines current-lines))
        (if (null? remaining)
          updated-lines
          (let ((detail (car remaining)))
            (if (detail-needs-rewrite? detail updated-lines)
              (let* ((env (env-detail-env detail))
                     (explicit-line (env-detail-explicit-rparen-line detail))
                     (tag-line (make-right-tag-line env))
                     (new-lines (list-set updated-lines (- explicit-line 1) tag-line)))
                (loop (cdr remaining) new-lines)
              ) ;let*
              (loop (cdr remaining) updated-lines)
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (insert-structural-right-tags current-lines details)
      (let ((to-insert (sort-details-for-structural-insert
                         (filter (lambda (detail)
                                   (detail-needs-structural-insert? detail current-lines details))
                                 details
                         ) ;filter     
                       )))
        (let loop ((remaining to-insert) (updated-lines current-lines))
          (if (null? remaining)
            updated-lines
            (let* ((detail (car remaining))
                   (env (env-detail-env detail))
                   (close-line (env-detail-close-line detail))
                   (tag-line (make-right-tag-line env))
                   (closed-line-idx (- close-line 1))
                   (closed-line (list-ref updated-lines closed-line-idx))
                   (trailing-unmatched-count
                    (line-trailing-unmatched-rparen-count closed-line)
                   ) ;trailing-unmatched-count
                   (new-lines
                    (if (and (line-starts-with-rparen? closed-line)
                             (= trailing-unmatched-count 1))
                      (list-set updated-lines closed-line-idx tag-line)
                      (let* ((shifted-line (remove-rparens-from-right-by-diff closed-line 1))
                             (shifted-lines (list-set updated-lines closed-line-idx shifted-line)))
                        (insert-line-at shifted-lines close-line tag-line)
                      ) ;let*
                    ) ;if
                   ) ;new-lines
                   )
              (loop (cdr remaining) new-lines)
            ) ;let*
          ) ;if
        ) ;let
    ) ;define
  ) ;begin

    (define (insert-single-line-of-right-tag lines)
      (let* ((initial-details (scan-environment-details lines))
             (rewritten-lines (rewrite-right-tags lines initial-details))
             (structurally-tagged-lines (insert-structural-right-tags rewritten-lines initial-details)))
        (if (check-lines-balanced lines)
          structurally-tagged-lines
          (let loop ((current-lines structurally-tagged-lines))
            (let* ((current-envs (scan-environments current-lines))
                   (remaining
                    (filter (lambda (env)
                              (and (not (env-in-prefixed-context? env current-lines))
                                   (env-needs-right-tag? env current-lines))
                              ) ;and
                            current-envs
                    ) ;filter
                   ) ;remaining
                   (total-lines (length current-lines)))
              (if (null? remaining)
                current-lines
                (let* ((env (choose-next-env remaining current-envs total-lines current-lines))
                       (raw-pos (find-insert-position env current-envs total-lines current-lines))
                       (pos (cond
                              ((number? raw-pos) raw-pos)
                              ((number? (env-lparen-line env)) (env-lparen-line env))
                              (else 0))
                       ) ;pos
                       (tag-line (make-right-tag-line env))
                       (new-lines (insert-line-at current-lines pos tag-line)))
                  (loop new-lines)
                ) ;let*
              ) ;if
            ) ;let*
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
