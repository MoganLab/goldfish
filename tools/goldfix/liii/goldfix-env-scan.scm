;;; Goldfix Env Scan 模块
;;; Environment 扫描入口
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env-scan)
  (import (scheme base))
  (import (liii string))
  (import (liii ascii))
  (import (liii goldfix-env-core))
  (import (liii goldfix-env-scan-step))

  (export scan-environment-details)
  (export scan-environments)
  (export scan-claimed-rparen-lines)

  (begin
    (define (env-has-indented-body-before-explicit-rparen? env explicit-line lines)
      (let ((start-line (+ (env-lparen-line env) 1))
            (env-col (env-lparen-col env)))
        (let loop ((line-num start-line))
          (if (>= line-num explicit-line)
            #f
            (let* ((line (list-ref lines (- line-num 1)))
                   (trimmed (string-trim line))
                   (col (- (string-length line) (string-length trimmed))))
              (if (or (string=? trimmed "")
                      (char=? (string-ref trimmed 0) #\;)
                      (ascii-right-paren? (string-ref trimmed 0)))
                (loop (+ line-num 1))
                (> col env-col)
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (apply-scan-detail-rparen! detail lines)
      (let* ((env (env-detail-env detail))
             (close-line (env-detail-close-line detail))
             (explicit-line (env-detail-explicit-rparen-line detail))
             (lparen-line (env-lparen-line env)))
        (cond
          ((and close-line
                (= close-line lparen-line)
                explicit-line
                (env-has-indented-body-before-explicit-rparen? env explicit-line lines))
           (env-set-rparen-line! env explicit-line)
          ) ;
          ((and close-line (= close-line lparen-line))
           (env-set-rparen-line! env close-line)
          ) ;
          (explicit-line
           (env-set-rparen-line! env explicit-line)
          ) ;explicit-line
          (else
           (env-set-rparen-line! env #f)
          ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (scan-source lines)
      (let loop ((remaining lines)
                 (line-num 1)
                 (stack '())
                 (block-depth 0)
                 (in-string #f)
                 (escape-next #f)
                 (envs '())
                 (details '())
                 (claimed-rparen-lines '()))
        (if (null? remaining)
          (let ((ordered-envs (reverse envs))
                (ordered-details (reverse details))
                (ordered-claimed-rparen-lines (reverse claimed-rparen-lines)))
            (for-each (lambda (detail)
                        (apply-scan-detail-rparen! detail lines))
                      ordered-details
            ) ;for-each
            (values ordered-envs ordered-details ordered-claimed-rparen-lines)
          ) ;let
          (let-values (((new-stack
                         new-block-depth
                         new-in-string
                         new-escape-next
                         new-envs
                         new-details
                         new-claimed-rparen-lines)
                        (scan-line (car remaining)
                                   lines
                                   line-num
                                   stack
                                   block-depth
                                   in-string
                                   escape-next
                                   envs
                                   details
                                   claimed-rparen-lines))
                        ) ;scan-line
            (loop (cdr remaining)
                  (+ line-num 1)
                  new-stack
                  new-block-depth
                  new-in-string
                  new-escape-next
                  new-envs
                  new-details
                  new-claimed-rparen-lines
            ) ;loop
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    (define (scan-environment-details lines)
      (let-values (((_envs details _claimed-rparen-lines) (scan-source lines)))
        details
      ) ;let-values
    ) ;define

    (define (scan-environments lines)
      (let-values (((envs _details _claimed-rparen-lines) (scan-source lines)))
        envs
      ) ;let-values
    ) ;define

    (define (scan-claimed-rparen-lines lines)
      (let-values (((_envs _details claimed-rparen-lines) (scan-source lines)))
        claimed-rparen-lines
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library
