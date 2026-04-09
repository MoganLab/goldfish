;;; Goldfix Raw String 模块
;;; raw-string 相关的行级词法状态辅助函数
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-raw-string)
  (import (scheme base))
  (import (liii goldfix-scheme))
  (import (liii goldfix-lint))

  (export line-start-state?)
  (export line-start-state-block-depth)
  (export line-start-state-in-string)
  (export line-start-state-escape-next)
  (export initial-line-start-state)
  (export advance-line-start-state)
  (export compute-line-start-states)
  (export line-net-open-count-with-state)
  (export line-net-open-count-at-state)
  (export line-touches-raw-string-with-state?)
  (export line-touches-raw-string-at-state?)

  (begin
    (define-record-type line-start-state
      (make-line-start-state block-depth in-string escape-next)
      line-start-state?
      (block-depth line-start-state-block-depth)
      (in-string line-start-state-in-string)
      (escape-next line-start-state-escape-next)
    ) ;define-record-type

    (define initial-line-start-state
      (make-line-start-state 0 #f #f)
    ) ;define

    (define (advance-line-start-state line state)
      (let-values (((_paren-counts next-block-depth next-in-string next-escape-next)
                    (count-parens-with-state line
                                             (line-start-state-block-depth state)
                                             (line-start-state-in-string state)
                                             (line-start-state-escape-next state)))
                    ) ;count-parens-with-state
        (make-line-start-state next-block-depth
                               next-in-string
                               next-escape-next
        ) ;make-line-start-state
      ) ;let-values
    ) ;define

    (define (compute-line-start-states lines)
      (let loop ((remaining lines)
                 (state initial-line-start-state)
                 (result '()))
        (if (null? remaining)
          (reverse result)
          (let ((line (car remaining)))
            (loop (cdr remaining)
                  (advance-line-start-state line state)
                  (cons state result)
            ) ;loop
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (line-net-open-count-with-state line block-depth in-string escape-next)
      (let-values (((paren-counts _next-block-depth _next-in-string _next-escape-next)
                    (count-parens-with-state line
                                             block-depth
                                             in-string
                                             escape-next))
                    ) ;count-parens-with-state
        (- (car paren-counts) (cdr paren-counts))
      ) ;let-values
    ) ;define

    (define (line-net-open-count-at-state line state)
      (line-net-open-count-with-state line
                                      (line-start-state-block-depth state)
                                      (line-start-state-in-string state)
                                      (line-start-state-escape-next state)
      ) ;line-net-open-count-with-state
    ) ;define

    ;; 阶段 2 的保守保护：
    ;; 只要这一行起始就在 raw-string 中，或扫描过程中进入了 raw-string，
    ;; 就视为“碰过 raw-string”。
    (define (line-touches-raw-string-with-state? line block-depth in-string escape-next)
      (let ((len (string-length line)))
        (let loop ((i 0)
                   (block-depth block-depth)
                   (in-string in-string)
                   (escape-next escape-next))
          (cond
            ((raw-string-state? in-string)
             #t
            ) ;
            ((>= i len)
             #f
            ) ;
            (else
             (let-values (((next-i next-block-depth next-in-string next-escape-next _mode)
                           (advance-lex-state line
                                              i
                                              block-depth
                                              in-string
                                              escape-next))
                           ) ;advance-lex-state
               (if (raw-string-state? next-in-string)
                 #t
                 (loop next-i
                       next-block-depth
                       next-in-string
                       next-escape-next
                 ) ;loop
               ) ;if
             ) ;let-values
            ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (line-touches-raw-string-at-state? line state)
      (line-touches-raw-string-with-state? line
                                           (line-start-state-block-depth state)
                                           (line-start-state-in-string state)
                                           (line-start-state-escape-next state)
      ) ;line-touches-raw-string-with-state?
    ) ;define
  ) ;begin
) ;define-library
