(define-library (liii goldfix-paren-core)
  (import (scheme base))
  (import (liii string))
  (import (liii ascii))
  (import (liii goldfix-env))
  (import (liii goldfix-lint))
  (import (liii goldfix-line))
  (import (liii goldfix-list))
  (import (only (liii list) filter find))

  (export env-end-line)
  (export find-next-sibling)
  (export find-insert-position)
  (export line-is-multi-line-start?)
  (export get-multi-line-envs)
  (export fix-env-parens)
  (export insert-line-at)
  (export remove-orphan-right-paren-lines)
  (export remove-rparens-from-right-by-diff)
  (export add-rparens-by-diff)

  (begin
    (define (env-end-line env envs total-lines)
      (let ((children (env-children env))
            (lparen-line (env-lparen-line env)))
        (if (or (not children) (null? children))
          lparen-line
          (let ((max-child-line (list-max children env-lparen-line)))
            (max (or max-child-line 0) lparen-line)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-next-sibling env envs)
      (let ((parent (env-parent env))
            (current-line (env-lparen-line env)))
        (let
          ((candidates
             (filter
               (lambda (e)
                 (and (eq? (env-parent e) parent)
                      (> (env-lparen-line e) current-line)
                 ) ;and
               ) ;lambda
               envs)
             ) ;filter
          ) ;
          (if (null? candidates)
            #f
            (let ((min-line (list-min candidates env-lparen-line)))
              (find (lambda (e) (= (env-lparen-line e) min-line)) candidates)
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (calculate-upper-bound parent envs total-lines)
      (if (not parent)
        total-lines
        (let ((parent-next (find-next-sibling parent envs))
              (parent-rline (env-rparen-line parent)))
          (let
            ((bound
              (cond
                (parent-rline parent-rline)
                ((and parent-next (env-rparen-line parent-next))
                 (env-rparen-line parent-next)
                ) ;
                ((and parent-next (env-lparen-line parent-next))
                 (env-lparen-line parent-next)
                ) ;
                (else total-lines))
              ) ;cond
            ) ;
            (if (number? bound) bound total-lines)
        ) ;let
      ) ;if
    ) ;define
  ) ;begin

    (define (calculate-base-insert-pos env next-sibling upper-bound)
      (let ((parent (env-parent env)))
        (if (and next-sibling (env-lparen-line next-sibling))
          (- (env-lparen-line next-sibling) 1)
          (if parent
            (- upper-bound 1)
            upper-bound
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (find-safe-insert-pos base-pos min-line lines)
      (let loop ((pos base-pos))
        (if (< pos min-line)
          base-pos
          (let ((line (list-ref lines (- pos 1))))
            (let ((trimmed (string-trim line)))
              (if (or (string-null? trimmed)
                      (char=? (string-ref trimmed 0) #\;))
                (loop (- pos 1))
                pos
              ) ;if
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (calculate-children-end env)
      (let ((children (env-children env)))
        (if (or (not children) (null? children))
          0
          (let ((max-rline (list-max children env-rparen-line)))
            (or max-rline 0)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (apply-parent-constraint result parent)
      (if (not parent)
        result
        (let ((parent-rline (env-rparen-line parent)))
          (if (and parent-rline (>= result parent-rline))
            (- parent-rline 1)
            result
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    (define (find-insert-position env envs total-lines lines)
      (let ((rline (env-rparen-line env))
            (lline (env-lparen-line env)))
        (cond
          ((and rline (> rline lline)) rline)
          (else
           (let* ((next-sibling (find-next-sibling env envs))
                  (parent (env-parent env))
                  (upper-bound (calculate-upper-bound parent envs total-lines))
                  (base-pos (calculate-base-insert-pos env next-sibling upper-bound))
                  (insert-pos (find-safe-insert-pos base-pos lline lines))
                  (children-end (calculate-children-end env))
                  (end-line (env-end-line env envs total-lines))
                  (result (max insert-pos (or end-line lline) children-end)))
             (apply-parent-constraint result parent)
           ) ;let*
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (line-is-multi-line-start? lines line-index envs)
      (let ((line (list-ref lines line-index)))
        (let ((trimmed (string-trim line)))
          (if (or (string-null? trimmed)
                  (not (ascii-left-paren? (string-ref trimmed 0))))
            #f
            (let ((line-num (+ line-index 1)))
              (let loop ((remaining envs))
                (if (null? remaining)
                  #f
                  (let ((env (car remaining)))
                    (if (and (= (env-lparen-line env) line-num)
                             (env-rparen-line env)
                             (> (env-rparen-line env) line-num))
                      #t
                      (loop (cdr remaining))
                    ) ;if
                  ) ;let
                ) ;if
              ) ;let
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (add-rparens-at-end line count)
      (if (<= count 0)
        line
        (let ((trimmed (string-trim-right line)))
          (string-append trimmed (make-string count #\)))
        ) ;let
      ) ;if
    ) ;define

    (define (get-multi-line-envs envs)
      (filter
        (lambda (e)
          (let ((rline (env-rparen-line e))
                (lline (env-lparen-line e)))
            (and rline lline (not (= rline lline)))
          ) ;let
        ) ;lambda
        envs
      ) ;filter
    ) ;define

    (define (fix-single-env! lines-vec m n)
      (let loop ((i m)
                 (accum 0)
                 (block-depth 0)
                 (in-string #f)
                 (escape-next #f))
        (if (> i n)
          lines-vec
          (let* ((line-idx (- i 1))
                 (line (vector-ref lines-vec line-idx)))
            (let-values (((paren-counts next-block-depth next-in-string next-escape-next)
                          (count-parens-with-state line
                                                   block-depth
                                                   in-string
                                                   escape-next)
                          ) ;count-parens-with-state
            ) ;let-values
              (let* ((lparen-count (car paren-counts))
                     (rparen-count (cdr paren-counts))
                     (diff (- lparen-count rparen-count))
                     (new-accum (+ accum diff))
                     (target-accum (if (< i n) 1 0)))
                (if (< new-accum target-accum)
                  (let ((remove-count (- target-accum new-accum)))
                    (if (> rparen-count 0)
                      (let ((new-line (remove-rparens-from-right-by-diff line remove-count)))
                        (vector-set! lines-vec line-idx new-line)
                        (loop (+ i 1)
                              target-accum
                              next-block-depth
                              next-in-string
                              next-escape-next
                        ) ;loop
                      ) ;let
                      (loop (+ i 1)
                            target-accum
                            next-block-depth
                            next-in-string
                            next-escape-next
                      ) ;loop
                    ) ;if
                  ) ;let
                  (loop (+ i 1)
                        new-accum
                        next-block-depth
                        next-in-string
                        next-escape-next
                  ) ;loop
                ) ;if
              ) ;let*
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    ) ;define

    (define (fix-env-parens lines envs)
      (let ((multi-envs (reverse (get-multi-line-envs envs)))
            (lines-vec (list->vector lines)))
        (let loop ((remaining-envs multi-envs))
          (if (null? remaining-envs)
            (vector->list lines-vec)
            (let* ((env (car remaining-envs))
                   (m (env-lparen-line env))
                   (n (env-rparen-line env)))
              (fix-single-env! lines-vec m n)
              (loop (cdr remaining-envs))
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (insert-line-at lines line-num content)
      (let loop ((rest lines) (current 0) (result '()))
        (cond
          ((= current line-num)
           (append (reverse (cons content result)) rest)
          ) ;
          ((null? rest)
           (reverse result)
          ) ;
          (else
           (loop (cdr rest)
                 (+ current 1)
                 (cons (car rest) result)
           ) ;loop
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (collect-rparen-line-flags envs claimed-rparen-lines total-lines)
      (let ((flags (make-vector total-lines #f)))
        (let loop ((rest envs))
          (if (null? rest)
            (let loop-claims ((remaining-claims claimed-rparen-lines))
              (if (null? remaining-claims)
                flags
                (let ((line-num (car remaining-claims)))
                  (when (and line-num (>= line-num 1) (<= line-num total-lines))
                    (vector-set! flags (- line-num 1) #t)
                  ) ;when
                  (loop-claims (cdr remaining-claims))
                ) ;let
              ) ;if
            ) ;let
            (let ((rline (env-rparen-line (car rest))))
              (when (and rline (>= rline 1) (<= rline total-lines))
                (vector-set! flags (- rline 1) #t)
              ) ;when
              (loop (cdr rest))
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (line-starts-with-rparen-in-code? line block-depth in-string escape-next)
      (if (or (> block-depth 0) in-string escape-next)
        #f
        (let ((len (string-length line)))
          (let loop ((i 0))
            (if (>= i len)
              #f
              (let ((ch (string-ref line i)))
                (cond
                  ((char-whitespace? ch)
                   (loop (+ i 1))
                  ) ;
                  ((and (< (+ i 1) len)
                        (char=? ch #\#)
                        (char=? (string-ref line (+ i 1)) #\|))
                   #f
                  ) ;
                  ((and (< (+ i 1) len)
                        (char=? ch #\#)
                        (char=? (string-ref line (+ i 1)) #\\))
                   #f
                  ) ;
                  ((char=? ch #\")
                   #f
                  ) ;
                  ((char=? ch #\;)
                   #f
                  ) ;
                  ((ascii-right-paren? ch)
                   #t
                  ) ;
                  (else
                   #f
                  ) ;else
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    (define (would-cause-imbalance-if-removed? result line remaining-lines block-depth in-string escape-next)
      ;; 检查删除当前行后，文件的括号是否仍然保持平衡
      ;; 策略：计算删除后的最终平衡，如果不为0或中间出现负数，则不能删除
      (let-values (((paren-counts _bd _is _en)
                    (count-parens-with-state line block-depth in-string escape-next)))
        (let ((lparen-count (car paren-counts))
              (rparen-count (cdr paren-counts)))
          ;; 计算已处理行的累计平衡
          (let loop-result ((remaining-result (reverse result))
                           (balance 0))
            (if (null? remaining-result)
              ;; 跳过当前行，检查剩余行
              (let loop-remaining ((remaining-rest remaining-lines)
                                  (current-balance balance)
                                  (bd block-depth)
                                  (is in-string)
                                  (en escape-next))
                (if (null? remaining-rest)
                  ;; 删除当前行后的最终平衡
                  ;; 应该等于：已处理 + 剩余 - 当前行的贡献
                  ;; = balance + current-balance - (lparen-count - rparen-count)
                  ;; = current-balance - lparen-count + rparen-count
                  (let ((final-balance current-balance))
                    ;; 如果最终平衡不为 0，说明删除会导致不平衡
                    (not (= final-balance 0))
                  ) ;let
                  (let-values (((counts next-bd next-is next-en)
                                (count-parens-with-state (car remaining-rest) bd is en)))
                    (let ((new-balance (+ current-balance (- (car counts) (cdr counts)))))
                      (if (< new-balance 0)
                        ;; 中间出现负平衡，不能删除
                        #t
                        (loop-remaining (cdr remaining-rest)
                                       new-balance
                                       next-bd
                                       next-is
                                       next-en)
                        ) ;loop-remaining
                    ) ;let
                  ) ;let-values
                ) ;if
              ) ;let
              (let-values (((counts _bd _is _en)
                            (count-parens-with-state (car remaining-result) 0 #f #f)))
                (let ((new-balance (+ balance (- (car counts) (cdr counts)))))
                  (if (< new-balance 0)
                    ;; 已处理行出现负平衡，不能删除
                    #t
                    (loop-result (cdr remaining-result) new-balance)
                  ) ;if
                ) ;let
              ) ;let-values
            ) ;if
          ) ;let
        ) ;let
      ) ;let-values
    ) ;define

    (define (remove-orphan-right-paren-lines lines envs)
      (let* ((line-count (length lines))
             (line-vec (list->vector lines))
             ;; env-rparen-line 只覆盖真正建了 env 的 form。
             ;; 像 (string-map (lambda ...)) 里的 ) ;lambda 这类 raw tagged close，
             ;; 需要额外依赖扫描阶段记录下来的显式右括号认领行。
             (claimed-rparen-lines (scan-claimed-rparen-lines lines))
             (rparen-flags (collect-rparen-line-flags envs claimed-rparen-lines line-count)))
        (let loop ((i 0)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f)
                   (result '()))
          (if (>= i line-count)
            (reverse result)
            (let ((line (vector-ref line-vec i)))
              (let-values (((_paren-counts next-block-depth next-in-string next-escape-next)
                            (count-parens-with-state line
                                                     block-depth
                                                     in-string
                                                     escape-next)
                            ) ;count-parens-with-state
              ) ;let-values
                ;; 获取剩余行列表（从 i+1 到结尾）
                (let
                  ((remaining-lines (let loop-remaining ((idx (+ i 1)) (acc '()))
                                      (if (>= idx line-count)
                                        (reverse acc)
                                        (loop-remaining (+ idx 1)
                                                       (cons (vector-ref line-vec idx) acc))
                                        ) ;loop-remaining
                                      ) ;if
                                    ) ;remaining-lines
                  ) ;
                (if (and (line-starts-with-rparen-in-code? line
                                                           block-depth
                                                           in-string
                                                           escape-next)
                         (not (vector-ref rparen-flags i))
                         (not (would-cause-imbalance-if-removed? result
                                                          line
                                                          remaining-lines
                                                          block-depth
                                                          in-string
                                                          escape-next))
                         ) ;not
                  (loop (+ i 1)
                        next-block-depth
                        next-in-string
                        next-escape-next
                        result
                  ) ;loop
                  (loop (+ i 1)
                        next-block-depth
                        next-in-string
                        next-escape-next
                        (cons line result)
                  ) ;loop
                ) ;if
              ) ;let
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define
    ) ;define

    (define (remove-rparens-from-right-by-diff line count)
      (if (<= count 0)
        line
        (remove-rparens-from-right line count)
      ) ;if
    ) ;define

    (define (add-rparens-by-diff line count)
      (if (<= count 0)
        line
        (add-rparens-at-end line count)
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
