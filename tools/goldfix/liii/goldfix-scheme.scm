;;; Goldfix Scheme 模块
;;; 共享的 Scheme 词法辅助函数
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-scheme)
  (import (scheme base))
  (import (liii ascii))

  ;; ---------- 导出接口 ----------
  (export char-identifier?)
  (export extract-identifier-tokens)
  (export char-literal-delimiter?)
  (export skip-char-literal-index)
  (export prefixed-open-index)

  (begin
    ;; 判断字符是否是合法的 Scheme 标识符字符
    ;; Scheme 标识符可以包含：字母、数字、以及 ! $ % & * + - . / : < = > ? @ ^ _ ~
    (define (char-identifier? ch)
      (or (char-alphabetic? ch)
          (char-numeric? ch)
          (char=? ch #\!)
          (char=? ch #\$)
          (char=? ch #\%)
          (char=? ch #\&)
          (char=? ch #\*)
          (char=? ch #\+)
          (char=? ch #\-)
          (char=? ch #\.)
          (char=? ch #\/)
          (char=? ch #\:)
          (char=? ch #\<)
          (char=? ch #\=)
          (char=? ch #\>)
          (char=? ch #\?)
          (char=? ch #\@)
          (char=? ch #\^)
          (char=? ch #\_)
          (char=? ch #\~)
      ) ;or
    ) ;define

    (define (extract-identifier-tokens str)
      (let ((len (string-length str)))
        (let loop ((i 0) (start #f) (tokens '()))
          (cond
            ((>= i len)
             (reverse
               (if start
                 (cons (substring str start len) tokens)
                 tokens
               ) ;if
             ) ;reverse
            ) ;
            ((char-identifier? (string-ref str i))
             (loop (+ i 1)
                   (or start i)
                   tokens
             ) ;loop
            ) ;
            (start
             (loop (+ i 1)
                   #f
                   (cons (substring str start i) tokens)
             ) ;loop
            ) ;start
            (else
             (loop (+ i 1) #f tokens)
            ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (char-literal-delimiter? ch)
      (or (char-whitespace? ch)
          (ascii-left-paren? ch)
          (ascii-right-paren? ch)
          (char=? ch #\")
          (char=? ch #\;)
      ) ;or
    ) ;define

    (define (skip-char-literal-index line start)
      (let ((len (string-length line)))
        (if (>= (+ start 2) len)
          len
          (let loop ((i (+ start 3)))
            (if (or (>= i len)
                    (char-literal-delimiter? (string-ref line i)))
              i
              (loop (+ i 1))
            ) ;if
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (prefixed-open-index line start)
      (let ((len (string-length line)))
        (let loop ((i start))
          (if (>= i len)
            #f
            (let ((ch (string-ref line i)))
              (cond
                ((char=? ch #\')
                 (loop (+ i 1))
                ) ;
                ((char=? ch #\`)
                 (loop (+ i 1))
                ) ;
                ((char=? ch #\,)
                 (if (and (< (+ i 1) len)
                          (char=? (string-ref line (+ i 1)) #\@))
                   (loop (+ i 2))
                   (loop (+ i 1))
                 ) ;if
                ) ;
                ((ascii-left-paren? ch)
                 i
                ) ;
                (else
                 #f
                ) ;else
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
