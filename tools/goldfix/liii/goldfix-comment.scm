;;; Goldfix Comment 模块
;;; 注释/字符串相关的行级辅助函数
;;;
;;; Copyright (c) 2024 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-comment)
  (import (scheme base))
  (import (liii string))

  ;; ---------- 导出接口 ----------
  (export line-ends-in-string-or-comment?)
  (export blank-or-comment-line?)

  (begin
    ;; 状态记录类型：字符处理状态
    (define-record-type char-state
      (make-char-state in-string in-comment escape-next)
      char-state?
      (in-string state-in-string)
      (in-comment state-in-comment)
      (escape-next state-escape-next)
    ) ;define-record-type

    ;; 创建初始状态
    (define (initial-char-state)
      (make-char-state #f #f #f)
    ) ;define

    ;; 处理下一个字符，返回新状态
    (define (next-state state ch)
      (cond
        ((state-escape-next state)
         (make-char-state (state-in-string state)
                          (state-in-comment state)
                          #f
         ) ;make-char-state
        ) ;
        ((state-in-string state)
         (cond
           ((char=? ch #\\) (make-char-state #t #f #t))
           ((char=? ch #\") (make-char-state #f #f #f))
           (else (make-char-state #t #f #f))
         ) ;cond
        ) ;
        ((state-in-comment state)
         (if (char=? ch #\newline)
           (make-char-state #f #f #f)
           (make-char-state #f #t #f)
         ) ;if
        ) ;
        (else
         (cond
           ((char=? ch #\") (make-char-state #t #f #f))
           ((char=? ch #\;) (make-char-state #f #t #f))
           (else state)
         ) ;cond
        ) ;else
      ) ;cond
    ) ;define

    ;; 检查行是否在字符串或注释中结束
    (define (line-ends-in-string-or-comment? line)
      (let loop ((chars (string->list line))
                 (state (initial-char-state)))
        (cond
          ((null? chars)
           (or (state-in-string state) (state-in-comment state))
          ) ;
          (else
           (loop (cdr chars) (next-state state (car chars)))
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 判断整行是否为空白或注释
    (define (blank-or-comment-line? line)
      (let ((trimmed (string-trim line)))
        (or (string-null? trimmed)
            (char=? (string-ref trimmed 0) #\;)
        ) ;or
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
