;;; Goldfix Env Scan Step 模块
;;; Environment 扫描分发逻辑
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env-scan-step)
  (import (scheme base))
  (import (liii string))
  (import (liii ascii))
  (import (liii goldfix-scheme))
  (import (liii goldfix-env-tag))
  (import (liii goldfix-env-core))
  (import (liii goldfix-env-right-tag))
  (import (liii goldfix-env-scan-state))

  (export scan-line)

  (begin
    (define (close-top-stack-node stack line-num)
      (if (null? stack)
        stack
        (let* ((node (car stack))
               (detail (paren-node-detail node)))
          (when (and detail
                     (not (env-detail-close-line detail)))
            (env-detail-set-close-line! detail line-num)
          ) ;when
          (cdr stack)
        ) ;let*
      ) ;if
    ) ;define

    (define (count-leading-rparens line)
      (let ((trimmed-line (string-trim line)))
        (let loop ((i 0))
          (if (and (< i (string-length trimmed-line))
                   (ascii-right-paren? (string-ref trimmed-line i)))
            (loop (+ i 1))
            i
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (close-leading-rparens stack line-num count)
      (let loop ((remaining count)
                 (current-stack stack)
                 (closed-any? #f))
        (if (or (<= remaining 0) (null? current-stack))
          (values current-stack closed-any?)
          (let ((next-stack (close-top-stack-node current-stack line-num)))
            (loop (- remaining 1)
                  next-stack
                  (or closed-any? (not (eq? next-stack current-stack)))
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (scan-leading-right-tag-line line lines line-num stack block-depth in-string escape-next envs details claimed-rparen-lines)
      (let*
        ((trimmed-line (string-trim line))
         (col (- (string-length line) (string-length trimmed-line)))
         (leading-rparen-count (count-leading-rparens line))
         (tag (extract-right-tag line))
         (tagged-open-stack (and (not (string-null? tag))
                                 (pop-open-right-tag-target-by-tag stack tag col line-num))
         ) ;tagged-open-stack
         (tagged-floating-target (and (not tagged-open-stack)
                                      (not (string-null? tag))
                                      (find-tagged-floating-right-tag-candidate lines details line-num col tag))
         ) ;tagged-floating-target
         (implicit-target (find-implicit-right-tag-target lines details line-num col))
        ) ;
        (define (claim-current-line)
          (cons line-num claimed-rparen-lines)
        ) ;define
        (cond
          (tagged-open-stack
           (scan-line-return tagged-open-stack
                             block-depth
                             in-string
                             escape-next
                             envs
                             details
                             (claim-current-line)
           ) ;scan-line-return
          ) ;tagged-open-stack
          (tagged-floating-target
           (env-detail-set-explicit-rparen-line! tagged-floating-target line-num)
           (scan-line-return stack
                             block-depth
                             in-string
                             escape-next
                             envs
                             details
                             (claim-current-line)
           ) ;scan-line-return
          ) ;tagged-floating-target
          (implicit-target
           (env-detail-set-explicit-rparen-line! implicit-target line-num)
           (scan-line-return stack
                             block-depth
                             in-string
                             escape-next
                             envs
                             details
                             (claim-current-line)
           ) ;scan-line-return
          ) ;implicit-target
          (else
           (if (string-null? tag)
             (let-values (((popped-stack closed-any?)
                           (close-leading-rparens stack line-num leading-rparen-count)))
               (scan-line-return popped-stack
                                 block-depth
                                 in-string
                                 escape-next
                                 envs
                                 details
                                 (if closed-any?
                                   (claim-current-line)
                                   claimed-rparen-lines
                                 ) ;if
               ) ;scan-line-return
             ) ;let-values
             (let ((popped-stack (close-top-stack-node stack line-num)))
               (scan-line-return popped-stack
                                 block-depth
                                 in-string
                                 escape-next
                                 envs
                                 details
                                 (if (eq? popped-stack stack)
                                   claimed-rparen-lines
                                   (claim-current-line)
                                 ) ;if
               ) ;scan-line-return
             ) ;let
           ) ;if
          ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    (define (scan-line-code-step line lines len line-num i ch stack block-depth in-string line-kind envs details claimed-rparen-lines lparen-count rparen-count first-rparen-detail)
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
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((char=? ch #\;)
         (scan-line-state len
                          stack
                          block-depth
                          in-string
                          #f
                          line-kind
                          envs
                          details
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((and (not line-kind) (char-whitespace? ch))
         (scan-line-state (+ i 1)
                          stack
                          block-depth
                          in-string
                          #f
                          line-kind
                          envs
                          details
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((and (not line-kind)
              (prefixed-open-index line i))
         (scan-line-open-form line
                              lines
                              len
                              line-num
                              i
                              stack
                              block-depth
                              in-string
                              envs
                              details
                              claimed-rparen-lines
                              lparen-count
                              rparen-count
                              first-rparen-detail
         ) ;scan-line-open-form
        ) ;
        ((and (not line-kind) (ascii-right-paren? ch))
         (scan-line-state i
                          stack
                          block-depth
                          in-string
                          #f
                          'rparen
                          envs
                          details
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((and (< (+ i 1) len)
              (char=? ch #\#)
              (char=? (string-ref line (+ i 1)) #\\))
         (scan-line-state (skip-char-literal-index line i)
                          stack
                          block-depth
                          in-string
                          #f
                          line-kind
                          envs
                          details
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((and (< (+ i 1) len)
              (char=? ch #\#)
              (char=? (string-ref line (+ i 1)) #\"))
         (scan-line-state (+ i 2)
                          stack
                          block-depth
                          (make-raw-string-delimiter-state "")
                          #f
                          line-kind
                          envs
                          details
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((and (not line-kind))
         (scan-line-atom-form line
                              lines
                              len
                              line-num
                              i
                              stack
                              block-depth
                              in-string
                              envs
                              details
                              claimed-rparen-lines
                              lparen-count
                              rparen-count
                              first-rparen-detail
         ) ;scan-line-atom-form
        ) ;
        ((char=? ch #\")
         (scan-line-state (+ i 1)
                          stack
                          block-depth
                          #t
                          #f
                          line-kind
                          envs
                          details
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;
        ((ascii-left-paren? ch)
         (scan-line-bare-lparen-step line
                                     len
                                     i
                                     line-num
                                     stack
                                     block-depth
                                     in-string
                                     line-kind
                                     envs
                                     details
                                     claimed-rparen-lines
                                     lparen-count
                                     rparen-count
                                     first-rparen-detail
         ) ;scan-line-bare-lparen-step
        ) ;
        ((ascii-right-paren? ch)
         (scan-line-close-rparen i
                                 line-num
                                 stack
                                 block-depth
                                 in-string
                                 line-kind
                                 envs
                                 details
                                 claimed-rparen-lines
                                 lparen-count
                                 rparen-count
                                 first-rparen-detail
         ) ;scan-line-close-rparen
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
                          claimed-rparen-lines
                          lparen-count
                          rparen-count
                          first-rparen-detail
         ) ;scan-line-state
        ) ;else
      ) ;cond
    ) ;define

    (define (scan-line line lines line-num stack block-depth in-string escape-next envs details claimed-rparen-lines)
      (let* ((len (string-length line))
             (trimmed-line (string-trim line)))
        (if (and (= block-depth 0)
                 (not in-string)
                 (not escape-next)
                 (not (string-null? trimmed-line))
                 (ascii-right-paren? (string-ref trimmed-line 0)))
          (scan-leading-right-tag-line line
                                       lines
                                       line-num
                                       stack
                                       block-depth
                                       in-string
                                       escape-next
                                       envs
                                       details
                                       claimed-rparen-lines
          ) ;scan-leading-right-tag-line
          (let loop ((i 0)
                     (stack stack)
                     (block-depth block-depth)
                     (in-string in-string)
                     (escape-next escape-next)
                     (line-kind #f)
                     (envs envs)
                     (details details)
                     (claimed-rparen-lines claimed-rparen-lines)
                     (lparen-count 0)
                     (rparen-count 0)
                     (first-rparen-detail #f))
            (if (>= i len)
              (finish-scan-line line
                                line-num
                                stack
                                block-depth
                                in-string
                                escape-next
                                envs
                                details
                                claimed-rparen-lines
                                line-kind
                                lparen-count
                                rparen-count
                                first-rparen-detail
              ) ;finish-scan-line
              (let ((ch (string-ref line i)))
                (define (continue-with next-i next-stack next-block-depth next-in-string next-escape-next next-line-kind next-envs next-details next-claimed-rparen-lines next-lparen-count next-rparen-count next-first-rparen-detail)
                  (loop next-i
                        next-stack
                        next-block-depth
                        next-in-string
                        next-escape-next
                        next-line-kind
                        next-envs
                        next-details
                        next-claimed-rparen-lines
                        next-lparen-count
                        next-rparen-count
                        next-first-rparen-detail
                  ) ;loop
                ) ;define
                (cond
                  (escape-next
                   (call-with-values
                     (lambda ()
                       (scan-line-state (+ i 1)
                                        stack
                                        block-depth
                                        in-string
                                        #f
                                        line-kind
                                        envs
                                        details
                                        claimed-rparen-lines
                                        lparen-count
                                        rparen-count
                                        first-rparen-detail
                       ) ;scan-line-state
                     ) ;lambda
                     continue-with
                   ) ;call-with-values
                  ) ;escape-next
                  ((> block-depth 0)
                   (call-with-values
                     (lambda ()
                       (scan-line-block-comment-step line
                                                     len
                                                     i
                                                     ch
                                                     stack
                                                     block-depth
                                                     in-string
                                                     line-kind
                                                     envs
                                                     details
                                                     claimed-rparen-lines
                                                     lparen-count
                                                     rparen-count
                                                     first-rparen-detail
                       ) ;scan-line-block-comment-step
                     ) ;lambda
                     continue-with
                   ) ;call-with-values
                  ) ;
                  ((raw-string-state? in-string)
                   (call-with-values
                     (lambda ()
                       (scan-line-raw-string-step line
                                                  i
                                                  stack
                                                  block-depth
                                                  in-string
                                                  line-kind
                                                  envs
                                                  details
                                                  claimed-rparen-lines
                                                  lparen-count
                                                  rparen-count
                                                  first-rparen-detail
                       ) ;scan-line-raw-string-step
                     ) ;lambda
                     continue-with
                   ) ;call-with-values
                  ) ;
                  (in-string
                   (call-with-values
                     (lambda ()
                       (scan-line-string-step i
                                              ch
                                              stack
                                              block-depth
                                              line-kind
                                              envs
                                              details
                                              claimed-rparen-lines
                                              lparen-count
                                              rparen-count
                                              first-rparen-detail
                       ) ;scan-line-string-step
                     ) ;lambda
                     continue-with
                   ) ;call-with-values
                  ) ;in-string
                  (else
                   (call-with-values
                     (lambda ()
                       (scan-line-code-step line
                                            lines
                                            len
                                            line-num
                                            i
                                            ch
                                            stack
                                            block-depth
                                            in-string
                                            line-kind
                                            envs
                                            details
                                            claimed-rparen-lines
                                            lparen-count
                                            rparen-count
                                            first-rparen-detail
                       ) ;scan-line-code-step
                     ) ;lambda
                     continue-with
                   ) ;call-with-values
                  ) ;else
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
