;;; Goldfix Line Scan 模块
;;; 行级括号扫描工具函数
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-line-scan)
  (import (scheme base))
  (import (liii string))
  (import (liii goldfix-constant))
  (import (liii goldfix-scheme))

  (export countable-rparen-indices)
  (export line-comment-start-index)
  (export unmatched-rparen-indices)
  (export count-trailing-rparen-indices)

  (begin
    ;; 找出一行中真正可删除的右括号位置。
    ;; 会跳过字符串、行注释、块注释以及字符字面量中的 )。
    (define (countable-rparen-indices line)
      (let ((len (string-length line)))
        (let loop ((i 0)
                   (indices '())
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f))
          (if (>= i len)
            (reverse indices)
            (let ((ch (string-ref line i)))
              (cond
                (escape-next
                 (loop (+ i 1)
                       indices
                       block-depth
                       in-string
                       #f
                 ) ;loop
                ) ;escape-next

                ((> block-depth 0)
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\|))
                    (loop (+ i 2)
                          indices
                          (+ block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\|)
                         (char=? (string-ref line (+ i 1)) #\#))
                    (loop (+ i 2)
                          indices
                          (- block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          indices
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;

                (in-string
                 (cond
                   ((char=? ch #\\)
                    (loop (+ i 1)
                          indices
                          block-depth
                          #t
                          #t
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          indices
                          block-depth
                          #f
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          indices
                          block-depth
                          #t
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;in-string

                (else
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\|))
                    (loop (+ i 2)
                          indices
                          (+ block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\\))
                    (loop (skip-char-literal-index line i)
                          indices
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          indices
                          block-depth
                          #t
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\;)
                    (reverse indices)
                   ) ;
                   ((char=? ch RPAREN)
                    (loop (+ i 1)
                          (cons i indices)
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          indices
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 找出行注释开始的位置。
    ;; 如果没有真正的 ; 注释，则返回行长度。
    (define (line-comment-start-index line)
      (let ((len (string-length line)))
        (let loop ((i 0)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f))
          (if (>= i len)
            len
            (let ((ch (string-ref line i)))
              (cond
                (escape-next
                 (loop (+ i 1)
                       block-depth
                       in-string
                       #f
                 ) ;loop
                ) ;escape-next

                ((> block-depth 0)
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\|))
                    (loop (+ i 2)
                          (+ block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\|)
                         (char=? (string-ref line (+ i 1)) #\#))
                    (loop (+ i 2)
                          (- block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;

                (in-string
                 (cond
                   ((char=? ch #\\)
                    (loop (+ i 1)
                          block-depth
                          #t
                          #t
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          block-depth
                          #f
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          block-depth
                          #t
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;in-string

                (else
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\|))
                    (loop (+ i 2)
                          (+ block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\\))
                    (loop (skip-char-literal-index line i)
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          block-depth
                          #t
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\;)
                    i
                   ) ;
                   (else
                    (loop (+ i 1)
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 找出一行里真正属于“净闭合外层环境”的右括号位置。
    ;; 这些 ) 在本行内部没有可配对的左括号，对应 line-net-close-count 的来源。
    (define (unmatched-rparen-indices line)
      (let ((len (string-length line)))
        (let loop ((i 0)
                   (indices '())
                   (open-count 0)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f))
          (if (>= i len)
            (reverse indices)
            (let ((ch (string-ref line i)))
              (cond
                (escape-next
                 (loop (+ i 1)
                       indices
                       open-count
                       block-depth
                       in-string
                       #f
                 ) ;loop
                ) ;escape-next

                ((> block-depth 0)
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\|))
                    (loop (+ i 2)
                          indices
                          open-count
                          (+ block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\|)
                         (char=? (string-ref line (+ i 1)) #\#))
                    (loop (+ i 2)
                          indices
                          open-count
                          (- block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          indices
                          open-count
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;

                (in-string
                 (cond
                   ((char=? ch #\\)
                    (loop (+ i 1)
                          indices
                          open-count
                          block-depth
                          #t
                          #t
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          indices
                          open-count
                          block-depth
                          #f
                          #f
                    ) ;loop
                   ) ;
                   (else
                    (loop (+ i 1)
                          indices
                          open-count
                          block-depth
                          #t
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;in-string

                (else
                 (cond
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\|))
                    (loop (+ i 2)
                          indices
                          open-count
                          (+ block-depth 1)
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((and (< (+ i 1) len)
                         (char=? ch #\#)
                         (char=? (string-ref line (+ i 1)) #\\))
                    (loop (skip-char-literal-index line i)
                          indices
                          open-count
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\")
                    (loop (+ i 1)
                          indices
                          open-count
                          block-depth
                          #t
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch #\;)
                    (reverse indices)
                   ) ;
                   ((char=? ch LPAREN)
                    (loop (+ i 1)
                          indices
                          (+ open-count 1)
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;
                   ((char=? ch RPAREN)
                    (if (> open-count 0)
                      (loop (+ i 1)
                            indices
                            (- open-count 1)
                            block-depth
                            in-string
                            #f
                      ) ;loop
                      (loop (+ i 1)
                            (cons i indices)
                            open-count
                            block-depth
                            in-string
                            #f
                      ) ;loop
                    ) ;if
                   ) ;
                   (else
                    (loop (+ i 1)
                          indices
                          open-count
                          block-depth
                          in-string
                          #f
                    ) ;loop
                   ) ;else
                 ) ;cond
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (count-trailing-rparen-indices line indices)
      (let ((comment-start (line-comment-start-index line)))
        (let loop ((i (- comment-start 1))
                   (remaining (reverse indices))
                   (count 0))
          (if (< i 0)
            count
            (let ((ch (string-ref line i)))
              (cond
                ((char-whitespace? ch)
                 (loop (- i 1) remaining count)
                ) ;
                ((and (pair? remaining)
                      (= i (car remaining)))
                 (loop (- i 1)
                       (cdr remaining)
                       (+ count 1)
                 ) ;loop
                ) ;
                (else
                 count
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
