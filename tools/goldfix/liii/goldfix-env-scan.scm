;;; Goldfix Env Scan 模块
;;; Environment 扫描入口
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env-scan)
  (import (scheme base))
  (import (liii goldfix-env-core))
  (import (liii goldfix-env-scan-step))

  (export scan-environment-details)
  (export scan-environments)

  (begin
    (define (apply-scan-detail-rparen! detail)
      (let* ((env (env-detail-env detail))
             (close-line (env-detail-close-line detail))
             (explicit-line (env-detail-explicit-rparen-line detail))
             (lparen-line (env-lparen-line env)))
        (cond
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
                 (details '()))
        (if (null? remaining)
          (let ((ordered-envs (reverse envs))
                (ordered-details (reverse details)))
            (for-each apply-scan-detail-rparen! ordered-details)
            (values ordered-envs ordered-details)
          ) ;let
          (let-values (((new-stack
                         new-block-depth
                         new-in-string
                         new-escape-next
                         new-envs
                         new-details)
                        (scan-line (car remaining)
                                   lines
                                   line-num
                                   stack
                                   block-depth
                                   in-string
                                   escape-next
                                   envs
                                   details))
                        ) ;scan-line
            (loop (cdr remaining)
                  (+ line-num 1)
                  new-stack
                  new-block-depth
                  new-in-string
                  new-escape-next
                  new-envs
                  new-details
            ) ;loop
          ) ;let-values
        ) ;if
      ) ;let
    ) ;define

    (define (scan-environment-details lines)
      (let-values (((_envs details) (scan-source lines)))
        details
      ) ;let-values
    ) ;define

    (define (scan-environments lines)
      (let-values (((envs _details) (scan-source lines)))
        envs
      ) ;let-values
    ) ;define

  ) ;begin
) ;define-library
