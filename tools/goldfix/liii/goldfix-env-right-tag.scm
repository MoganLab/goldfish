;;; Goldfix Env Right Tag 模块
;;; 右标记匹配与浮动候选选择
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env-right-tag)
  (import (scheme base))
  (import (liii string))
  (import (liii goldfix-comment))
  (import (liii ascii))
  (import (liii goldfix-env-core))

  (export find-implicit-right-tag-target)
  (export pop-open-right-tag-target-by-tag)
  (export pop-open-right-tag-target)
  (export find-tagged-floating-right-tag-candidate)

  (begin
    (define (line-rparen-col line)
      (let ((trimmed (string-trim line)))
        (if (or (string-null? trimmed)
                (not (ascii-right-paren? (string-ref trimmed 0))))
          #f
          (- (string-length line) (string-length trimmed))
        ) ;if
      ) ;let
    ) ;define

    (define (detail-explicit-at-line? details line-num)
      (let loop ((rest details))
        (if (null? rest)
          #f
          (let ((explicit-line (env-detail-explicit-rparen-line (car rest))))
            (if (and explicit-line (= explicit-line line-num))
              #t
              (loop (cdr rest))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (compatible-between-line? line env-col)
      (let ((rparen-col (line-rparen-col line)))
        (or (blank-or-comment-line? line)
            (and rparen-col
                 (> rparen-col env-col)
            ) ;and
        ) ;or
      ) ;let
    ) ;define

    (define (blank-or-comment-only-between? lines start-line end-line env-col)
      (let loop ((line-num start-line))
        (if (> line-num end-line)
          #t
          (if (compatible-between-line? (list-ref lines (- line-num 1)) env-col)
            (loop (+ line-num 1))
            #f
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (better-floating-candidate? a b)
      (let ((a-close (env-detail-close-line a))
            (b-close (env-detail-close-line b))
            (a-env (env-detail-env a))
            (b-env (env-detail-env b)))
        (or (> a-close b-close)
            (and (= a-close b-close)
                 (> (env-lparen-line a-env) (env-lparen-line b-env))
            ) ;and
        ) ;or
      ) ;let
    ) ;define

    (define (better-tagged-floating-candidate? a b)
      (let* ((a-close (env-detail-close-line a))
             (b-close (env-detail-close-line b))
             (a-env (env-detail-env a))
             (b-env (env-detail-env b))
             (a-col (env-lparen-col a-env))
             (b-col (env-lparen-col b-env)))
        (or (> a-close b-close)
            (and (= a-close b-close)
                 (or (> a-col b-col)
                     (and (= a-col b-col)
                          (> (env-lparen-line a-env) (env-lparen-line b-env))
                     ) ;and
                 ) ;or
            ) ;and
        ) ;or
      ) ;let*
    ) ;define

    (define (find-floating-right-tag-candidate lines details line-num col)
      (let loop ((rest details) (best #f))
        (if (null? rest)
          best
          (let* ((detail (car rest))
                 (env (env-detail-env detail))
                 (close-line (env-detail-close-line detail))
                 (explicit-line (env-detail-explicit-rparen-line detail))
                 (lparen-line (env-lparen-line env))
                 (lparen-col (env-lparen-col env)))
            (if (and close-line
                     (not explicit-line)
                     (> close-line lparen-line)
                     (< close-line line-num)
                     (= lparen-col col)
                     (blank-or-comment-only-between? lines (+ close-line 1) (- line-num 1) lparen-col)
                     (or (not best)
                         (better-floating-candidate? detail best)
                     ) ;or
            ) ;if
              (loop (cdr rest) detail)
              (loop (cdr rest) best)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
  ) ;begin

    (define (find-tagged-floating-right-tag-candidate lines details line-num col tag)
      (let loop ((rest details) (best #f))
        (if (null? rest)
          best
          (let* ((detail (car rest))
                 (env (env-detail-env detail))
                 (close-line (env-detail-close-line detail))
                 (explicit-line (env-detail-explicit-rparen-line detail))
                 (lparen-line (env-lparen-line env))
                 (lparen-col (env-lparen-col env)))
            (if (and close-line
                     (not explicit-line)
                     (> close-line lparen-line)
                     (< close-line line-num)
                     (string=? (env-tag env) tag)
                     (<= lparen-col col)
                     (blank-or-comment-only-between? lines (+ close-line 1) (- line-num 1) lparen-col)
                     (or (not best)
                         (better-tagged-floating-candidate? detail best)
                     ) ;or
            ) ;if
              (loop (cdr rest) detail)
              (loop (cdr rest) best)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
) ;define-library

    (define (find-implicit-right-tag-target lines details line-num col)
      (let loop ((rest details) (best #f))
        (if (null? rest)
          best
          (let* ((detail (car rest))
                 (env (env-detail-env detail))
                 (close-line (env-detail-close-line detail))
                 (explicit-line (env-detail-explicit-rparen-line detail))
                 (lparen-line (env-lparen-line env))
                 (lparen-col (env-lparen-col env)))
            (if (and close-line
                     (not explicit-line)
                     (> close-line lparen-line)
                     (< close-line line-num)
                     (= lparen-col col)
                     (blank-or-comment-only-between? lines
                                                     (+ close-line 1)
                                                     (- line-num 1)
                                                     lparen-col
                     ) ;blank-or-comment-only-between?
                     (or (not best)
                         (better-floating-candidate? detail best)
                     ) ;or
            ) ;if
              (loop (cdr rest) detail)
              (loop (cdr rest) best)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define
    ) ;define

    (define (pop-open-right-tag-target-by-tag stack tag col line-num)
      ;; 对带 tag 的右标记，优先要求与左括号列号精确匹配。
      ;; 如果这里做“最近列号”的模糊匹配，像嵌套 if/let 这类重复 tag
      ;; 的场景会把右标记误吸附到更外层 env，并顺手把中间栈帧一并弹掉。
      ;; 当没有精确匹配的 open env 时，scan-line 会继续尝试 tagged floating fallback。
      (let loop ((rest stack) (depth 0))
        (cond
          ((null? rest)
           #f
          ) ;
          (else
           (let* ((node (car rest))
                  (detail (paren-node-detail node))
                  (env (paren-node-env node))
                  (raw-tag (paren-node-tag node)))
             (cond
               ((and detail
                     (= (env-lparen-col env) col)
                     (string=? (env-tag env) tag)
                ) ;and
                (env-detail-set-close-line! detail line-num)
                (env-detail-set-explicit-rparen-line! detail line-num)
                (list-tail stack (+ depth 1))
               ) ;
               ;; 对同一行里的嵌套 form（如 (string-map (lambda ...))），
               ;; 我们不会为 lambda 建 env-detail，但右标记仍应先消费掉这个裸括号节点，
               ;; 不能误把外层 string-map 提前弹出。
               ((and (not detail)
                     (= (paren-node-col node) col)
                     (not (string-null? raw-tag))
                     (string=? raw-tag tag)
                ) ;and
                (list-tail stack (+ depth 1))
               ) ;
               (else
                (loop (cdr rest) (+ depth 1))
               ) ;else
             ) ;cond
           ) ;let*
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (pop-open-right-tag-target stack col line-num)
      (define (pop-by-match pred)
        (let loop ((rest stack) (depth 0))
          (cond
            ((null? rest)
             #f
            ) ;
            ((and (paren-node-detail (car rest))
                  (pred (env-lparen-col (paren-node-env (car rest)))))
             (let ((detail (paren-node-detail (car rest))))
               (env-detail-set-close-line! detail line-num)
               (env-detail-set-explicit-rparen-line! detail line-num)
               (list-tail stack (+ depth 1))
             ) ;let
            ) ;
            (else
             (loop (cdr rest) (+ depth 1))
            ) ;else
          ) ;cond
        ) ;let
      ) ;define

      (or (pop-by-match (lambda (env-col) (= env-col col)))
          (pop-by-match (lambda (env-col) (<= env-col col)))
          stack
      ) ;or
    ) ;define

    ;; 目前 attach-floating-right-tags! 仍保留在这里，便于后续在扫描后阶段直接复用。
    (define (attach-floating-right-tags! lines details)
      (let loop ((line-num 1) (remaining lines))
        (when (not (null? remaining))
          (let* ((line (car remaining))
                 (col (line-rparen-col line)))
            (when (and col
                       (not (detail-explicit-at-line? details line-num)))
              (let ((candidate (find-floating-right-tag-candidate lines details line-num col)))
                (when candidate
                  (env-detail-set-explicit-rparen-line! candidate line-num)
                ) ;when
              ) ;let
            ) ;when
            (loop (+ line-num 1) (cdr remaining))
          ) ;let*
        ) ;when
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
