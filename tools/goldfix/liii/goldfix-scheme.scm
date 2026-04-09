;;; Goldfix Scheme 模块
;;; 共享的 Scheme 词法辅助函数
;;;
;;; Copyright (c) 2026 Liii Network
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
  (export raw-string-state?)
  (export raw-string-state-phase)
  (export raw-string-state-delimiter)
  (export raw-string-state-match-index)
  (export make-raw-string-delimiter-state)
  (export make-raw-string-body-state)
  (export raw-string-state-delimiter-phase?)
  (export raw-string-state-body-phase?)
  (export advance-lex-state)
  (export advance-lex-state-at-line-break)

  (begin
    (define-record-type raw-string-state
      (make-raw-string-state phase delimiter match-index)
      raw-string-state?
      (phase raw-string-state-phase)
      (delimiter raw-string-state-delimiter)
      (match-index raw-string-state-match-index)
    ) ;define-record-type

    (define (make-raw-string-delimiter-state delimiter)
      (make-raw-string-state 'delimiter delimiter 0)
    ) ;define

    (define (make-raw-string-body-state delimiter match-index)
      (make-raw-string-state 'body delimiter match-index)
    ) ;define

    (define (raw-string-state-delimiter-phase? state)
      (and (raw-string-state? state)
           (eq? (raw-string-state-phase state) 'delimiter)
      ) ;and
    ) ;define

    (define (raw-string-state-body-phase? state)
      (and (raw-string-state? state)
           (eq? (raw-string-state-phase state) 'body)
      ) ;and
    ) ;define

    (define (raw-string-closer-length delimiter)
      (+ (string-length delimiter) 2)
    ) ;define

    (define (raw-string-closer-char delimiter idx)
      (let ((delimiter-len (string-length delimiter)))
        (cond
          ((= idx 0)
           #\"
          ) ;
          ((<= idx delimiter-len)
           (string-ref delimiter (- idx 1))
          ) ;
          (else
           #\"
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (raw-string-next-match delimiter match-index ch)
      (let ((expected (raw-string-closer-char delimiter match-index)))
        (if (char=? ch expected)
          (+ match-index 1)
          (if (char=? ch #\")
            1
            0
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (raw-string-step state ch)
      (if (raw-string-state-delimiter-phase? state)
        (if (char=? ch #\")
          (make-raw-string-body-state
            (raw-string-state-delimiter state)
            0
          ) ;make-raw-string-body-state
          (make-raw-string-delimiter-state
            (string-append (raw-string-state-delimiter state)
                           (string ch)
            ) ;string-append
          ) ;make-raw-string-delimiter-state
        ) ;if
        (let* ((delimiter (raw-string-state-delimiter state))
               (match-index (raw-string-state-match-index state))
               (next-match (raw-string-next-match delimiter match-index ch)))
          (if (= next-match (raw-string-closer-length delimiter))
            #f
            (make-raw-string-body-state delimiter next-match)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

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

    ;; 推进一行中的词法状态。
    ;; 返回：
    ;; - 下一索引
    ;; - 下一 block-depth
    ;; - 下一 in-string 状态（#f / #t / raw-string-state）
    ;; - 下一 escape-next
    ;; - 模式：'code-char / 'skip-char / 'line-comment
    (define (advance-lex-state line i block-depth in-string escape-next)
      (let* ((len (string-length line))
             (ch (string-ref line i)))
        (cond
          (escape-next
           (values (+ i 1)
                   block-depth
                   in-string
                   #f
                   'skip-char
           ) ;values
          ) ;escape-next
          ((> block-depth 0)
           (cond
             ((and (< (+ i 1) len)
                   (char=? ch #\#)
                   (char=? (string-ref line (+ i 1)) #\|))
              (values (+ i 2)
                      (+ block-depth 1)
                      in-string
                      #f
                      'skip-char
              ) ;values
             ) ;
             ((and (< (+ i 1) len)
                   (char=? ch #\|)
                   (char=? (string-ref line (+ i 1)) #\#))
              (values (+ i 2)
                      (- block-depth 1)
                      in-string
                      #f
                      'skip-char
              ) ;values
             ) ;
             (else
              (values (+ i 1)
                      block-depth
                      in-string
                      #f
                      'skip-char
              ) ;values
             ) ;else
           ) ;cond
          ) ;
          ((raw-string-state? in-string)
           (let ((next-state (raw-string-step in-string ch)))
             (values (+ i 1)
                     block-depth
                     (if next-state next-state #f)
                     #f
                     'skip-char
             ) ;values
           ) ;let
          ) ;
          ((eq? in-string #t)
           (cond
             ((char=? ch #\\)
              (values (+ i 1)
                      block-depth
                      #t
                      #t
                      'skip-char
              ) ;values
             ) ;
             ((char=? ch #\")
              (values (+ i 1)
                      block-depth
                      #f
                      #f
                      'skip-char
              ) ;values
             ) ;
             (else
              (values (+ i 1)
                      block-depth
                      #t
                      #f
                      'skip-char
              ) ;values
             ) ;else
           ) ;cond
          ) ;
          (else
           (cond
             ((and (< (+ i 1) len)
                   (char=? ch #\#)
                   (char=? (string-ref line (+ i 1)) #\|))
              (values (+ i 2)
                      (+ block-depth 1)
                      in-string
                      #f
                      'skip-char
              ) ;values
             ) ;
             ((and (< (+ i 1) len)
                   (char=? ch #\#)
                   (char=? (string-ref line (+ i 1)) #\\))
              (values (skip-char-literal-index line i)
                      block-depth
                      in-string
                      #f
                      'skip-char
              ) ;values
             ) ;
             ((and (< (+ i 1) len)
                   (char=? ch #\#)
                   (char=? (string-ref line (+ i 1)) #\"))
              (values (+ i 2)
                      block-depth
                      (make-raw-string-delimiter-state "")
                      #f
                      'skip-char
              ) ;values
             ) ;
             ((char=? ch #\")
              (values (+ i 1)
                      block-depth
                      #t
                      #f
                      'skip-char
              ) ;values
             ) ;
             ((char=? ch #\;)
              (values len
                      block-depth
                      in-string
                      #f
                      'line-comment
              ) ;values
             ) ;
             (else
              (values (+ i 1)
                      block-depth
                      in-string
                      #f
                      'code-char
              ) ;values
             ) ;else
           ) ;cond
          ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 推进逻辑行尾状态。
    ;; 这里将换行视作 raw-string 的正文或 delimiter 内容，
    ;; 这样跨行 raw-string 的匹配进度可以被正确保留。
    (define (advance-lex-state-at-line-break block-depth in-string escape-next)
      (cond
        ((raw-string-state? in-string)
         (if (raw-string-state-delimiter-phase? in-string)
           (values block-depth
                   (make-raw-string-delimiter-state
                     (string-append (raw-string-state-delimiter in-string)
                                    (string #\newline)
                     ) ;string-append
                   ) ;make-raw-string-delimiter-state
                   #f
           ) ;values
           (let ((next-state (raw-string-step in-string #\newline)))
             (values block-depth
                     (if next-state next-state #f)
                     #f
             ) ;values
           ) ;let
         ) ;if
        ) ;
        (else
         (values block-depth in-string escape-next)
        ) ;else
      ) ;cond
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
