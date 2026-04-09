(define-library (liii goldfix-split)
  (import (scheme base))
  (import (liii string))
  (import (liii goldfix-constant))
  (import (liii goldfix-env))
  (import (liii goldfix-lint))
  (import (liii goldfix-line-scan))
  (import (liii goldfix-split-cond))
  (import (liii goldfix-scheme))

  (export normalize-multi-open-lines)

  (begin
    (define (normalize-restart-index changed-pass-index)
      (cond
        ;; binding 的拆分会继续暴露新的 binding 行，直接回到 binding 本身
        ((= changed-pass-index 1) 1)
        ;; map/filter -> lambda 的拆分只会继续暴露后续 nested-lambda
        ((= changed-pass-index 2) 2)
        ;; case 头部拆开后，下一步主要是 clause/body 级别的拆分
        ((= changed-pass-index 3) 4)
        ;; clause body 被拆到新行后，可能露出 let/map/filter/case 等更早阶段
        ((= changed-pass-index 4) 1)
        ;; let header 换行主要会继续影响外层 let header，本阶段自收敛即可
        ((= changed-pass-index 5) 5)
        (else 1)
      ) ;cond
    ) ;define

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

    (define (scope-covers-entire-env? scope)
      (or (eq? scope 'env)
          (eq? scope 'env-selective)
      ) ;or
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
      (if (scope-covers-entire-env? (split-plan-scope plan))
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

    (define (shift-lines-indentation-selectively lines delta threshold)
      (map
        (lambda (line)
          (if (>= (line-leading-space-count line) threshold)
            (shift-line-indentation line delta)
            line
          ) ;if
        ) ;lambda
        lines
      ) ;map
    ) ;define

    (define (rewrite-split-pass lines envs collect-plan)
      (let loop ((remaining-lines lines)
                 (remaining-envs envs)
                 (line-num 1)
                 (block-depth 0)
                 (in-string #f)
                 (escape-next #f)
                 (result '())
                 (changed? #f))
        (if (null? remaining-lines)
          (if changed?
            (values (reverse result) #t)
            (values lines #f)
          ) ;if
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
                         (if (eq? (split-plan-scope plan) 'env-selective)
                           (shift-lines-indentation-selectively
                             (cdr consumed-lines)
                             (split-plan-indent-delta plan)
                             (split-plan-start-index plan)
                           ) ;shift-lines-indentation-selectively
                           (shift-lines-indentation
                             (cdr consumed-lines)
                             (split-plan-indent-delta plan)
                           ) ;shift-lines-indentation
                         ) ;if
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
                            #t
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
                      changed?
                ) ;loop
              ) ;let-values
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (apply-split-pass lines cached-envs collect-plan)
      (let
        ((envs
           (if cached-envs
             cached-envs
             (scan-environments lines)
           ) ;if
         ) ;envs
        ) ;
        (call-with-values
          (lambda ()
            (rewrite-split-pass lines envs collect-plan)
          ) ;lambda
          (lambda (next-lines changed?)
            (values next-lines
                    (if changed? #f envs)
                    changed?
            ) ;values
          ) ;lambda
        ) ;call-with-values
      ) ;let
    ) ;define

    (define (normalize-multi-open-lines lines)
      (let
        ((pass-order
           (list (list 1 "binding" collect-binding-split-plan)
                 (list 2 "nested-lambda" nested-lambda-split-plan)
                 (list 3 "case-inline-clause" case-inline-clause-split-plan)
                 (list 4 "conditional-clause" conditional-clause-split-plan)
                 (list 5 "binding-list-header" binding-list-header-split-plan)
           ) ;list
         ) ;pass-order
        ) ;
        ;; 不同 pass 之间的“新机会”传播方向并不相同，
        ;; 改动后只回跳到最早可能重新受影响的阶段，而不是一律回到开头。
        (let loop ((current-lines lines)
                   (current-envs #f)
                   (remaining-steps 20)
                   (remaining-passes pass-order))
          (if (or (= remaining-steps 0)
                  (null? remaining-passes))
            current-lines
            (let* ((pass (car remaining-passes))
                   (pass-position (car pass))
                   (collect-plan (caddr pass))
                   ) ;collect-plan
              (call-with-values
                (lambda ()
                  (apply-split-pass
                    current-lines
                    current-envs
                    collect-plan
                  ) ;apply-split-pass
                ) ;lambda
                (lambda (next-lines next-envs changed?)
                  (if changed?
                    (loop next-lines
                          #f
                          (- remaining-steps 1)
                          (list-tail pass-order
                                     (- (normalize-restart-index
                                          pass-position)
                                        1
                                     ) ;-
                          ) ;list-tail
                    ) ;loop
                    (loop next-lines
                          next-envs
                          (- remaining-steps 1)
                          (cdr remaining-passes)
                    ) ;loop
                  ) ;if
                ) ;lambda
              ) ;call-with-values
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
