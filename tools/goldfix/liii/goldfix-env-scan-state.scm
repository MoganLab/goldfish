;;; Goldfix Env Scan State 模块
;;; Environment 扫描状态推进原语
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env-scan-state)
  (import (scheme base))
  (import (liii goldfix-constant))
  (import (liii goldfix-scheme))
  (import (liii goldfix-env-tag))
  (import (liii goldfix-env-core))

  (export scan-line-return)
  (export finish-scan-line)
  (export scan-line-open-form)
  (export scan-line-atom-form)
  (export scan-line-close-rparen)
  (export scan-line-state)
  (export scan-line-block-comment-step)
  (export scan-line-string-step)
  (export scan-line-bare-lparen-step)

  (begin
    (define (scan-line-return stack block-depth in-string escape-next envs details)
      (values stack block-depth in-string escape-next envs details)
    ) ;define

    (define (finish-scan-line line-num stack block-depth in-string escape-next envs details line-kind lparen-count rparen-count first-rparen-detail)
      (when (and (eq? line-kind 'rparen)
                 (= lparen-count 0)
                 (= rparen-count 1)
                 first-rparen-detail)
        (env-detail-set-explicit-rparen-line! first-rparen-detail line-num)
      ) ;when
      (scan-line-return stack block-depth in-string escape-next envs details)
    ) ;define

    (define (scan-line-open-form line lines len line-num i stack block-depth in-string envs details lparen-count rparen-count first-rparen-detail)
      (let* ((open-index (prefixed-open-index line i))
             (trimmed (substring line open-index len))
             (tag (extract-tag trimmed))
             (effective-stack (unwind-stack-before-col lines stack open-index line-num))
             (parent (find-open-parent-by-col effective-stack open-index))
             (env (make-env tag line-num open-index parent))
             (detail (make-env-detail env #f #f)))
        (add-child! parent env)
        (values (+ open-index 1)
                (cons (make-paren-node line-num open-index env detail) stack)
                block-depth
                in-string
                #f
                'lparen
                (cons env envs)
                (cons detail details)
                (+ lparen-count 1)
                rparen-count
                first-rparen-detail
        ) ;values
      ) ;let*
    ) ;define

    (define (scan-line-atom-form line lines len line-num i stack block-depth in-string envs details lparen-count rparen-count first-rparen-detail)
      (let* ((trimmed (substring line i len))
             (tag (extract-first-token trimmed))
             (effective-stack (unwind-stack-before-col lines stack i line-num))
             (parent (find-open-parent-by-col effective-stack i))
             (env (make-env tag line-num i parent)))
        (add-child! parent env)
        (env-set-rparen-line! env line-num)
        (values i
                stack
                block-depth
                in-string
                #f
                'atom
                (cons env envs)
                details
                lparen-count
                rparen-count
                first-rparen-detail
        ) ;values
      ) ;let*
    ) ;define

    (define (scan-line-close-rparen i line-num stack block-depth in-string line-kind envs details lparen-count rparen-count first-rparen-detail)
      (let ((new-rparen-count (+ rparen-count 1)))
        (if (null? stack)
          (values (+ i 1)
                  stack
                  block-depth
                  in-string
                  #f
                  line-kind
                  envs
                  details
                  lparen-count
                  new-rparen-count
                  first-rparen-detail
          ) ;values
          (let* ((node (car stack))
                 (rest-stack (cdr stack))
                 (detail (paren-node-detail node))
                 (candidate-detail (if (and (eq? line-kind 'rparen)
                                            (= rparen-count 0))
                                     detail
                                     first-rparen-detail))
                 ) ;candidate-detail
            (when (and detail
                       (not (env-detail-close-line detail)))
              (env-detail-set-close-line! detail line-num)
            ) ;when
            (values (+ i 1)
                    rest-stack
                    block-depth
                    in-string
                    #f
                    line-kind
                    envs
                    details
                    lparen-count
                    new-rparen-count
                    candidate-detail
            ) ;values
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define (scan-line-state i stack block-depth in-string escape-next line-kind envs details lparen-count rparen-count first-rparen-detail)
      (values i
              stack
              block-depth
              in-string
              escape-next
              line-kind
              envs
              details
              lparen-count
              rparen-count
              first-rparen-detail
      ) ;values
    ) ;define

    (define (scan-line-block-comment-step line len i ch stack block-depth in-string line-kind envs details lparen-count rparen-count first-rparen-detail)
      (cond
        ((and (< (+ i 1) len)
              (char=? ch #\#)
              (char=? (string-ref line (+ i 1)) #\|))
         (scan-line-state (+ i 2)
                          stack
                          (+ block-depth 1)
                          in-string
                          #f
                          line-kind
                          envs
                          details
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((and (< (+ i 1) len)
              (char=? ch #\|)
              (char=? (string-ref line (+ i 1)) #\#))
         (scan-line-state (+ i 2)
                          stack
                          (- block-depth 1)
                          in-string
                          #f
                          line-kind
                          envs
                          details
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        (else
         (scan-line-state (+ i 1)
                          stack
                          block-depth
                          in-string
                          #f
                          line-kind
                          envs
                          details
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;else
      ) ;cond
    ) ;define

    (define (scan-line-string-step i ch stack block-depth line-kind envs details lparen-count rparen-count first-rparen-detail)
      (cond
        ((char=? ch #\\)
         (scan-line-state (+ i 1)
                          stack
                          block-depth
                          #t
                          #t
                          line-kind
                          envs
                          details
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((char=? ch #\")
         (scan-line-state (+ i 1)
                          stack
                          block-depth
                          #f
                          #f
                          line-kind
                          envs
                          details
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        (else
         (scan-line-state (+ i 1)
                          stack
                          block-depth
                          #t
                          #f
                          line-kind
                          envs
                          details
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;else
      ) ;cond
    ) ;define

    (define (scan-line-bare-lparen-step i line-num stack block-depth in-string line-kind envs details lparen-count rparen-count first-rparen-detail)
      (scan-line-state (+ i 1)
                       (cons (make-paren-node line-num i #f #f) stack)
                       block-depth
                       in-string
                       #f
                       line-kind
                       envs
                       details
                       (+ lparen-count 1)
                       rparen-count
                       first-rparen-detail
      ) ;scan-line-state
    ) ;define

  ) ;begin
) ;define-library
