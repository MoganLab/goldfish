;;; Goldfix Env Tag 模块
;;; 标签提取与右标记文本生成
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env-tag)
  (import (scheme base))
  (import (liii string))
  (import (liii ascii))
  (import (liii goldfix-scheme))
  (import (liii goldfix-env-core))

  (export extract-first-token)
  (export extract-tag)
  (export extract-right-tag)
  (export make-right-tag-line)

  (begin
    ;; 从行中提取第一个 token（空格或括号分隔的第一个词）
    (define (extract-first-token line)
      (if (string-null? line)
        ""
        (let ((len (string-length line)))
          (let loop ((i 0) (start #f))
            (cond
              ((>= i len)
               (if start (substring line start len) "")
              ) ;
              ((and start
                    (or (char-whitespace? (string-ref line i))
                        (ascii-left-paren? (string-ref line i))
                        (ascii-right-paren? (string-ref line i))
                    ) ;or
              ) ;
               (substring line start i)
            ) ;cond
              ((and (not start) (not (char-whitespace? (string-ref line i))))
               (loop (+ i 1) i)
              ) ;
              (else
               (loop (+ i 1) start)
              ) ;else
          ) ;let
        ) ;let
      ) ;if
    ) ;define
  ) ;begin

    (define (filler-token? token)
      (or (string=? token "end")
          (string=? token "of")
          (string=? token "the")
          (string=? token "internal")
      ) ;or
    ) ;define

    (define (preferred-tag-from-tokens tokens)
      (let ((meaningful
             (let loop ((rest tokens) (result '()))
               (if (null? rest)
                 (reverse result)
                 (if (filler-token? (car rest))
                   (loop (cdr rest) result)
                   (loop (cdr rest) (cons (car rest) result))
                 ) ;if
               ) ;if
             ) ;let
            )) ;meaningful
        (cond
          ((null? meaningful) "")
          ((null? (cdr meaningful)) (car meaningful))
          (else
           (let loop ((rest meaningful))
             (cond
               ((null? rest)
                (car (reverse meaningful))
               ) ;
               ((or (string-index (car rest) #\-)
                    (string-index (car rest) #\*))
                (car rest)
               ) ;
               (else
                (loop (cdr rest))
               ) ;else
             ) ;cond
           ) ;let
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 从行首提取 tag
    (define (extract-tag line)
      (let ((trimmed (string-trim line)))
        (if (or (string-null? trimmed)
                (not (ascii-left-paren? (string-ref trimmed 0))))
          ""
          (let ((len (string-length trimmed)))
            (let loop ((i 1) (start #f) (end #f))
              (cond
                (end
                 (substring trimmed start end)
                ) ;end
                ((>= i len)
                 (if start
                   (substring trimmed start len)
                   ""
                 ) ;if
                ) ;
                ((and (not start) (char-whitespace? (string-ref trimmed i)))
                 (loop (+ i 1) start end)
                ) ;
                ((and (not start) (ascii-left-paren? (string-ref trimmed i)))
                 ""
                ) ;
                ((and (not start) (char-identifier? (string-ref trimmed i)))
                 (loop (+ i 1) i end)
                ) ;
                ((and start
                      (not end)
                      (not (char-identifier? (string-ref trimmed i))))
                 (loop (+ i 1) start i)
                ) ;
                (else
                 (loop (+ i 1) start end)
                ) ;else
              ) ;cond
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 从右标记行提取 tag
    (define (extract-right-tag line)
      (let ((trimmed (string-trim line)))
        (if (or (string-null? trimmed)
                (not (ascii-right-paren? (string-ref trimmed 0))))
          ""
          (let ((len (string-length trimmed)))
            (let loop ((i 1))
              (cond
                ((>= i len) "")
                ((char=? (string-ref trimmed i) #\;)
                 (preferred-tag-from-tokens
                   (extract-identifier-tokens
                     (substring trimmed (+ i 1) len)
                   ) ;extract-identifier-tokens
                 ) ;preferred-tag-from-tokens
                ) ;
                (else
                 (loop (+ i 1))
                ) ;else
              ) ;cond
            ) ;let
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 生成右标记行内容
    (define (make-right-tag-line env)
      (let ((col (env-lparen-col env))
            (tag (env-tag env)))
        (let ((indent (make-string col #\space)))
          (if (string-null? tag)
            (string-append indent ") ;")
            (string-append indent ") ;" tag)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

) ;define-library
) ;define-library
